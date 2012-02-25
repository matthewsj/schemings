#lang racket

(require mzlib/list)

;; =============================================================================
;; TODO:
;; * docs [already started]
;; * custom printer for units
;; * library of units
;; * better way to specify families? (relationships via other units, e.g. inches
;;      in a foot -> feet in a mile
;; * conversions between families for different scales on the same measure (e.g.
;;      metric vs english distance units
;; * ability to define derived units? e.g. Hz = 1/seconds, and allow printer
;;      or some other explicit conversion between the two
;; * ability to scale units to some nice scale. this is problematic with derived
;;      units.
;; * should there be a way to extract subunits? it would be good if unit scaling
;;      could be written in user code.

;; =============================================================================
;; UNITS OF MEASURE

;; base-unit ::= (base-unit symbol symbol real)
;; unit ::= (unit-of-measure (listof base-unit) (listof base-unit))
;;   where:
;;   * no base-unit is repeated in the numerator and denominator, and
;;   * all units of the same family are expressed with the same unit, and
;;   * within the numerator and the denominator, base units are ordered by
;;   * family<?.

(struct base-unit (family name conversion-weight) #:transparent)

(define (write-unit unit out write?)
  (fprintf out (format-unit unit)))
(struct unit-of-measure 
  (numerator denominator)
  #:property prop:procedure (λ (unit quantity) (measure quantity unit))
  #:property prop:custom-write write-unit
  #:property prop:equal+hash
  (list
   (λ (a b equal?)
     (and (equal? (unit-of-measure-numerator a) (unit-of-measure-numerator b))
          (equal? (unit-of-measure-denominator a) (unit-of-measure-denominator b))))
   (λ (unit subhash)
     (subhash (cons (unit-of-measure-numerator unit) (unit-of-measure-denominator unit))))
   (λ (unit subhash)
     (subhash (cons (unit-of-measure-numerator unit) (unit-of-measure-denominator unit))))))

(define (format-unit unit)
  (cond
    [(and (null? (unit-of-measure-numerator unit))
          (null? (unit-of-measure-denominator unit)))
     ""]
    [(and (null? (unit-of-measure-numerator unit))
          (not (null? (unit-of-measure-denominator unit))))
     (string-append "1/" (format-base-units (unit-of-measure-denominator unit)))]
    [(and (not (null? (unit-of-measure-numerator unit)))
          (null? (unit-of-measure-denominator unit)))
     (format-base-units (unit-of-measure-numerator unit))]
    [else
     (string-append (format-base-units (unit-of-measure-numerator unit))
                    "/"
                    (format-base-units (unit-of-measure-denominator unit)))]))

(define (format-base-units base-units)
  (string-join (map (λ (u) (symbol->string (base-unit-name u))) base-units)
               " "))

(struct measure (quantity unit) #:transparent)

(define measure-or-number/c (or/c measure? number?))
(define unit-or-measure/c (or/c measure-or-number/c unit-of-measure?))

(provide 
 define-units-of-measure
 unit-of-measure?
 (contract-out
  [unitless unit-of-measure?]
  [inverse (-> unit-of-measure? unit-of-measure?)]
  [unit-of-measure-family (->* (symbol?) #:rest (listof (list/c symbol? number?))
                               (listof unit-of-measure?))]
  [multiply-units (->* () #:rest (listof unit-of-measure?) unit-of-measure?)]
  [divide-units (-> unit-of-measure? unit-of-measure? unit-of-measure?)]
  [conversion-factor (-> unit-of-measure? unit-of-measure? number?)])
 (struct-out measure)
 (struct-out exn:fail:contract:conversion)
 (contract-out
  [convert (-> measure? unit-of-measure? measure?)]
  [plus-measures (->* () #:rest (listof measure-or-number/c) measure-or-number/c)]
  [minus-measures (-> measure-or-number/c measure-or-number/c measure-or-number/c)]
  [times-measures (->* () #:rest (listof measure-or-number/c) measure-or-number/c)]
  [divide-measures (-> measure-or-number/c measure-or-number/c measure-or-number/c)]
  
  [plus (->* () #:rest (listof unit-or-measure/c) unit-or-measure/c)]
  [minus (-> unit-or-measure/c unit-or-measure/c unit-or-measure/c)]
  [times (->* () #:rest (listof unit-or-measure/c) unit-or-measure/c)]
  [divided-by (-> unit-or-measure/c unit-or-measure/c unit-or-measure/c)]))

(define (family<? a b)
  (string<? (symbol->string (base-unit-family a))
            (symbol->string (base-unit-family b))))

;; Trivial unit that represents scalar quantities.
(define unitless (unit-of-measure '() '()))

(define (inverse u)
  (unit-of-measure (unit-of-measure-denominator u)
                   (unit-of-measure-numerator u)))

;; unit-of-measure-family : symbol (list symbol number)... -> (values unit...)
(define (unit-of-measure-family family-name . members)   
  (for/list ([m members])
    (unit-of-measure 
     (list (base-unit family-name (car m) (cadr m))) 
     '())))

(define-syntax-rule (define-units-of-measure family-name (member-name member-scale) ...)
  (define-values (member-name ...)
    (apply values (unit-of-measure-family 'family-name (list 'member-name member-scale) ...))))

;; flip : (a b -> c) -> b a -> c
(define ((flip f) a b) (f b a))

;; base-units-of : unit -> (seteq-of base-unit)
;; Returns the set of all distinct base units in the given unit. Guarantees that
;; each unit in the return value has a distinct family.
(define (base-units-of unit)
  (set-union
   (apply seteq (unit-of-measure-numerator unit))
   (apply seteq (unit-of-measure-denominator unit))))

;; convert-base-units : unit (seteq-of base-unit) -> unit
;; Side constraint: each base-unit in the input set must have a distinct family.
;; Converts every base-unit within unit that shares a family with a base unit in conversions
;; to the instance of the family that appears in conversions.
(define (convert-base-units unit conversions)  
  (define conversion-map 
    (for/hasheq ([base-unit conversions])
      (values (base-unit-family base-unit) base-unit)))
  (define (conv base-unit)
    (let ([conversion (hash-ref conversion-map (base-unit-family base-unit) #f)])
      (or conversion base-unit)))
  (unit-of-measure
   (map conv (unit-of-measure-numerator unit))
   (map conv (unit-of-measure-denominator unit))))

;; cancel : (listof X) (listof X) -> (values (listof X) (listof X))
;; Returns the two inputs with all common values removed. Each occurence of an element in one list
;; cancels a single occurence in the other list;
;;   e.g. (cancel (list a a) (list a)) produces (values (list a) (list)).
(define (cancel a b)
  (values (remove-from a b) (remove-from b a)))

;; remove-from : (listof X) (listof X) -> (listof X))
;; Removes one element from the first list for each matching element in the second.
(define (remove-from a b)
  (cond
    [(null? b) a]
    [else (remove-from (remove (car b) a) (cdr b))]))

;; multiply-units/2 : unit unit -> unit
;; Returns the product of two units.
(define (multiply-units/2 a b)  
  (let*-values ([(b) (convert-base-units b (base-units-of a))]
                [(residual-numerator-a residual-denominator-b)
                 (cancel (unit-of-measure-numerator a)
                         (unit-of-measure-denominator b))]
                [(residual-numerator-b residual-denominator-a)
                 (cancel (unit-of-measure-numerator b)
                         (unit-of-measure-denominator a))])
    (unit-of-measure (merge-sorted-lists residual-numerator-a residual-numerator-b family<?)
                     (merge-sorted-lists residual-denominator-a residual-denominator-b family<?))))

;; multiply-units : unit ... -> unit
;; Returns the product of all units.
(define (multiply-units . units)
  (foldl (flip multiply-units/2) unitless units))

;; divide-units : unit unit -> unit
;; Returns the quotient of the input units.
(define (divide-units a b)
  (multiply-units/2 a (inverse b)))

(struct exn:fail:contract:conversion exn:fail:contract
  (from to)
  #:transparent)

;; conversion-factor/list : (listof base-unit) (listof base-unit) -> number
;; side constraint: both lists are sorted by base unit, in family<? order, and
;; within an input each unit of a given family is expressed in the same units.
;;
;; Determines the conversion factor needed to convert a measure expressed as the
;; product of each unit in from into a measure expressed as the product of each
;; unit in to. Raises exn:fail:contract:conversion if the units are not convertible.
(define (conversion-factor/list from to)
  (foldl (λ (f t prev-factor)
           (unless (eq? (base-unit-family f) (base-unit-family t))
             (raise (exn:fail:contract:conversion
                     (format "Could not convert unit ~s to ~s" (base-unit-name f) (base-unit-name t))
                     (current-continuation-marks)
                     (unit-of-measure (list f) '())
                     (unit-of-measure (list t) '()))))
           (* (/ (base-unit-conversion-weight f) (base-unit-conversion-weight t))
              prev-factor))
         1
         from to))

;; conversion-factor : unit unit -> number
;; Returns the scalar by which a measure expressed in the first unit
;; should be scaled to produce the same measure expressed in the second unit.
;; Raises exn:fail:contract:conversion if the units are not convertible.
(define (conversion-factor from to)
  (/ (conversion-factor/list (unit-of-measure-numerator from)
                             (unit-of-measure-numerator to))
     (conversion-factor/list (unit-of-measure-denominator from)
                             (unit-of-measure-denominator to))))

;; =============================================================================
;; MEASURES

;; convert : measure canonical-form-unit -> measure
;; converts the given measure to the given unit. Raises exn:fail:contract:conversion
;; if the measure cannot be converted to the desired unit.
(define (convert original-measure target-unit)
  (measure 
   (* (measure-quantity original-measure) 
      (conversion-factor (measure-unit original-measure) target-unit))
   target-unit))

;; plus-measures : measure... -> measure
;; sums the given measures and converts them to the left-most units.
;; Raises exn:fail:contract:conversion if the units are not all convertible to common units.
(define (plus-measures . args)
  (define (+/2 b a)  ; note args are flipped so that "lefthand-units win" rule works for plus
    (let (; slight optimization: don't call convert and build the intermediate measure, just
          ; do the conversion inline
          [conversion-factor (conversion-factor (measure-unit b) (measure-unit a))])
      (measure (+ (measure-quantity a) (* (measure-quantity b) conversion-factor))
               (measure-unit a))))
  (cond
    [(null? args) 0] ; unclear what to do with this case.
    [else (foldl +/2 (car args) (cdr args))]))

(define (minus-measures a b)
  (plus-measures a (measure (- (measure-quantity b)) (measure-unit b))))

;; as-measure : (or measure number) -> measure
(define (as-measure v)
  (cond
    [(number? v) (measure v unitless)]
    [else v]))
  
;; to-measure-or-scalar : measure -> (or measure number)
;; If the given measure is unitless, returns the scalar it represents. 
;; Otherwise returns the entire measure.
(define (to-measure-or-scalar m)
  (if (equal? (measure-unit m) unitless)
      (measure-quantity m)
      m))

;; times-measures : (or measure number)... -> (or measure number)
;; returns the product of the given measures and scalars.
(define (times-measures . args)
  (define (*/2 b a) ; note args are flipped for "lefthand-units win" rule
    (let* ([a (as-measure a)]
           [b (as-measure b)]
           [a-unit (measure-unit a)]
           [b-unit (measure-unit b)]
           [b-unit-expressed-with-a-units 
            (convert-base-units b-unit (base-units-of a-unit))]
           [b (convert b b-unit-expressed-with-a-units)])
      (measure 
       (* (measure-quantity a) (measure-quantity b))
       (multiply-units/2 (measure-unit a) b-unit-expressed-with-a-units))))  
  (to-measure-or-scalar (foldl */2 (measure 1 unitless) args)))

;; divide-measures : (or measure number) (or measure number) -> (or measure number)
(define (divide-measures a b)
  (let ([a (as-measure a)]
        [b (as-measure b)])
    (times-measures
     a 
     (measure (/ 1 (measure-quantity b)) (inverse (measure-unit b))))))

;; =============================================================================
;; Convenience methods for working with units, measures, and scalars as
;; appropriate.

(define (measure-or-number? v)
  (or (measure? v) (number? v)))

(define (plus . args)
  (cond
    [(null? args) 0]
    [(andmap measure? args)
     (apply plus-measures args)]
    [else (apply + args)]))

(define (minus a b)
  (cond
    [(and (measure? a) (measure? b))
     (minus-measures a b)]
    [else (- a b)]))

(define (times . args)
  (cond
    [(null? args) 1]
    [(andmap unit-of-measure? args)
     (apply multiply-units args)]
    [(andmap measure-or-number? args)
     (apply times-measures args)]
    [else (apply * args)]))

(define (divided-by a b)
  (cond
    [(and (unit-of-measure? a) (unit-of-measure? b))
     (divide-units a b)]
    [(and (measure-or-number? a) (measure-or-number? b))
     (divide-measures a b)]
    [else (/ a b)]))