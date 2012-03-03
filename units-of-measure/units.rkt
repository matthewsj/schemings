#lang racket

(require mzlib/list)

;; =============================================================================
;; TODO:
;; * docs [already started]
;; * library of units
;; * better way to specify families? (relationships via other units, e.g. inches
;;      in a foot -> feet in a mile
;; =============================================================================
;; UNITS OF MEASURE
;;
;; Terminology:
;;    - A _dimension_ is a measurable quantity, e.g. distance, time.
;;    - A _scale_ is a family of base units that measure some dimension,
;;          e.g. imperial or metric units of distance.
;;    - A _base unit_ is a unit in a scale; e.g. meters, feet.
;;    - A _derived unit_ is a unit formed by the multiplication and division
;;          of base units, e.g. meters/second^2.

;; dimension ::= (dimension symbol (listof scale))
;; scale ::= (scale symbol dimension hasheq[scale -o> conversion])
;; conversion ::= (conversion base-unit base-unit real)
;; base-unit ::= (base-unit scale symbol real)
;; unit ::= (unit-of-measure (listof base-unit) (listof base-unit))
;;   where:
;;   * no base-unit is repeated in the numerator and denominator, and
;;   * all units of the same scale are expressed with the same unit, and
;;   * within the numerator and the denominator, base units are ordered by
;;   * scale<?.
(struct dimension (name [scales #:mutable]))
(struct scale (name dimension [conversions #:mutable]))
(struct conversion (from to scaling-factor))
(struct base-unit (scale name conversion-weight) #:transparent)

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
 define-dimension
 define-scale
 unit-of-measure?
 (contract-out
  [unitless unit-of-measure?]
  [inverse (-> unit-of-measure? unit-of-measure?)]
  [multiply-units (->* () #:rest (listof unit-of-measure?) unit-of-measure?)]
  [divide-units (-> unit-of-measure? unit-of-measure? unit-of-measure?)]
  [conversion-factor (-> unit-of-measure? unit-of-measure? number?)]
  [set-conversion! (-> unit-of-measure? real? unit-of-measure? any)])
 (struct-out dimension)
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

(define (scale<? a b)
  (string<? (symbol->string (scale-name (base-unit-scale a)))
            (symbol->string (scale-name (base-unit-scale b)))))

;; Trivial unit that represents scalar quantities.
(define unitless (unit-of-measure '() '()))

(define (inverse u)
  (unit-of-measure (unit-of-measure-denominator u)
                   (unit-of-measure-numerator u)))

(define-syntax-rule (define-dimension name)
  (define name (dimension 'name '())))

(define-syntax-rule (define-scale dimension scale-name (member-name member-scale) ...)
  (define-values (scale-name member-name ...)
    (let ([d dimension])
      (unless (dimension? d)
        (raise (exn:fail:contract
                (format "Expected a dimension, given: ~e" d)
                (current-continuation-marks))))
      (let* ([scale (scale 'scale-name d (hasheq))]
             [units (for/list ([m (list (list 'member-name member-scale) ...)])
                      (unit-of-measure
                       (list (base-unit scale (car m) (cadr m))) 
                       '()))])
        (set-dimension-scales! d (cons scale (dimension-scales d)))
        (apply values scale units)))))

;; flip : (a b -> c) -> b a -> c
(define ((flip f) a b) (f b a))

;; base-units-of : unit -> (seteq-of base-unit)
;; Returns the set of all distinct base units in the given unit. Guarantees that
;; each unit in the return value has a distinct scale.
(define (base-units-of unit)
  (set-union
   (apply seteq (unit-of-measure-numerator unit))
   (apply seteq (unit-of-measure-denominator unit))))

;; extract-base-unit : unit -> base-unit
;; gets the underlying base unit representation of a base unit represented in canonical form.
(define (extract-base-unit unit)
  (unless (and (eq? (length (unit-of-measure-numerator unit)) 1)
               (null? (unit-of-measure-denominator unit)))
    (raise (exn:fail:contract "Not a base unit" (current-continuation-marks))))
  (car (unit-of-measure-numerator unit)))

;; convert-base-units : unit (seteq-of base-unit) -> unit
;; Side constraint: each base-unit in the input set must have a distinct scale.
;; Converts every base-unit within unit that shares a scale with a base unit in conversions
;; to the instance of the scale that appears in conversions.
(define (convert-base-units unit conversions)  
  (define conversion-map 
    (for/hasheq ([base-unit conversions])
      (values (base-unit-scale base-unit) base-unit)))
  (define (conv base-unit)
    (let ([conversion (hash-ref conversion-map (base-unit-scale base-unit) #f)])
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
    (unit-of-measure (merge-sorted-lists residual-numerator-a residual-numerator-b scale<?)
                     (merge-sorted-lists residual-denominator-a residual-denominator-b scale<?))))

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

;; =============================================================================
;; Scale conversions
;; We want to allow users to define new conversion types, and have those types
;; interconvert with existing types. For that, we create a registry of all
;; dimensions, and for each dimension associate a set of scales. When necessary
;; we can convert from scale to scale while keeping the same dimension.

(define (dimension-of base-unit)
  (scale-dimension (base-unit-scale base-unit)))

(define (set-conversion! from-unit/nonbase scaling-factor to-unit/nonbase)
  (let ([from-unit (extract-base-unit from-unit/nonbase)]
        [to-unit (extract-base-unit to-unit/nonbase)])
    (unless (eq? (dimension-of from-unit)
                 (dimension-of to-unit))
      (raise (exn:fail:contract
              (format "Unit ~s measures dimension ~s, which is incompatible with unit ~s, which measures dimension ~s"
                      (base-unit-name from-unit)
                      (dimension-name (dimension-of from-unit))
                      (base-unit-name to-unit)
                      (dimension-name (dimension-of to-unit)))
              (current-continuation-marks))))
    (when (eq? (base-unit-scale from-unit) (base-unit-scale to-unit))
      (raise (exn:fail:contract
              "Cannot set conversion of units within the same scale"
              (current-continuation-marks))))
  
    ;; TODO: Once base scales are updated, compute transitive closure. This will require
    ;; validity checking (i.e. verify that the new conversion is compatible with the
    ;; conversions we have already learned about, and abort the whole thing if not)
    (let ([d (dimension-of from-unit)]
          [from-scale (base-unit-scale from-unit)]
          [to-scale (base-unit-scale to-unit)])
      (set-scale-conversions! from-scale 
                              (hash-set (scale-conversions from-scale)
                                        to-scale (conversion from-unit to-unit scaling-factor)))
      (set-scale-conversions! to-scale 
                              (hash-set (scale-conversions to-scale)
                                        from-scale (conversion to-unit from-unit (/ 1 scaling-factor)))))))

;; convertible-base-units? : base-unit base-unit -> boolean?
(define (convertible-base-units? from to)
  (or (eq? (base-unit-scale from) (base-unit-scale to))
      (hash-has-key? (scale-conversions (base-unit-scale from)) (base-unit-scale to))))

;; conversion-factor/within-scale : base-unit base-unit -> real?
;; computes the conversion factor within a single scale
(define (conversion-factor/within-scale f t)
  (/ (base-unit-conversion-weight f) (base-unit-conversion-weight t)))

(define (base-unit-conversion-factor from to)
  (cond
    [(eq? (base-unit-scale from) (base-unit-scale to))
     (conversion-factor/within-scale from to)]
    [(hash-ref (scale-conversions (base-unit-scale from)) (base-unit-scale to) #f)
     =>
     (lambda (conversion)
       (* (conversion-factor/within-scale from (conversion-from conversion))
          (conversion-scaling-factor conversion)
          (conversion-factor/within-scale (conversion-to conversion) to)))]
    [else 
     (raise 
      (exn:fail:contract:conversion
       (format "Could not convert unit ~s to ~s" (base-unit-name from) (base-unit-name to))
       (current-continuation-marks)
       (unit-of-measure (list from) '())
       (unit-of-measure (list to) '())))]))

;; conversion-factor/list : (listof base-unit) (listof base-unit) -> number
;; side constraint: both lists are sorted by base unit, in scale<? order, and
;; within an input each unit of a given scale is expressed in the same units.
;;
;; Determines the conversion factor needed to convert a measure expressed as the
;; product of each unit in from into a measure expressed as the product of each
;; unit in to. Raises exn:fail:contract:conversion if the units are not convertible.
(define (conversion-factor/list from to)
  (foldl (λ (f t prev-factor)           
           (* (base-unit-conversion-factor f t) prev-factor))
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

;; convert : measure unit -> measure
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