#lang racket
(require slideshow/pict)

;; ============================================================
;; A LIBRARY FOR DISTRIBUTING PICTS ALONG PATHS

;; pth ::= (pth (real? -> real?) (real? -> real?) real? real?)
;; a description of a finite (vector) path: a function that maps from
;; "time" to x-coordinate, another that maps from "time" to y-coordinate,
;; and minimum and maximum times that the path considers. (Here and
;; elsewhere, for clarity we describe picts "travelling" across a
;; given path over "time"; by this we just mean that we call "time"
;; the input to our path's component functions.)
(struct pth (fx fy min max))

(define pict-or-pict-fn/c (or/c pict? (-> real? pict?)))
(provide (contract-out
          (struct pth 
            ((fx (-> real? real?))
             (fy (-> real? real?))
             (min real?)
             (max real?)))
          [path-between (->* (pict? pict? pict?)
                             (#:origin-locator (-> pict? pict? (values real? real?))
                              #:destination-locator (-> pict? pict? (values real? real?)))
                             pth?)]
          [distribute-between (->* (pict? pict? pict? (listof pict-or-pict-fn/c))
                             (#:origin-locator (-> pict? pict? (values real? real?))
                              #:destination-locator (-> pict? pict? (values real? real?))
                              #:rotate boolean?)
                             pict?)]
          [distribute (->* (pth? (listof pict-or-pict-fn/c))
                           (#:divide (symbols 'evenly-across-domain 'evenly-across-range)
                            #:rotate boolean?)
                           pict?)]))

;; Internal representation of a mapping between a time and its corresponding x and y coordinates.
(struct pt (t x y))

;; path-between : pic pict pict [#:origin-locator xx-find] [#:destination-locator xx-find] -> pth?
;; Returns a path whose distance and direction corresponds to the (vector) distance from the first
;; pict to the second within the given scene.
(define (path-between origin destination overall-pict
                      #:origin-locator [find-origin-pt rc-find]
                      #:destination-locator [find-destination-pt lc-find])
  (let-values ([(origin-x origin-y) (find-origin-pt overall-pict origin)]
               [(destination-x destination-y) (find-destination-pt overall-pict destination)])
    (let* ([function-domain-endpoint 1000]
           [scaled (λ (start end)
                     (let ([delta (- end start)])
                       (λ (t) (* delta (/ t function-domain-endpoint)))))])                    
    (pth (scaled origin-x destination-x)
         (scaled (- origin-y) (- destination-y))  ;; xx-find functions return inverted points
         0
         function-domain-endpoint))))

(define (distribute-between origin destination overall-pict objects-to-distribute
                            #:origin-locator [find-origin-pt rc-find]
                            #:destination-locator [find-destination-pt lc-find]
                            #:rotate [rotate? #f])
  (let* ([path (path-between origin destination overall-pict
                             #:origin-locator find-origin-pt
                             #:destination-locator find-destination-pt)]
         [distributed-pict (distribute path objects-to-distribute #:rotate rotate?)])
    (pin-over overall-pict origin find-origin-pt distributed-pict)))

(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p1))))))

(define (pairwise-distances samples)
  (let loop ([lefts samples]
             [rights (cdr samples)])
    (cond
      [(null? rights) '()]
      [else
       (cons (distance (car lefts) (car rights)) 
             (loop (cdr lefts) (cdr rights)))])))

(define epsilon 1)
(define (within-tolerance? n)
  (< (abs n) epsilon))

;; get-equal-domain-points : pth nat[>=2] -> (listof pt)
;; Returns the result of evaluating the pth function across n equally-spaced
;; points
(define (evenly-across-domain pth n)
  (let* ([time-to-travel (- (pth-max pth) (pth-min pth))]
         [each-delta-t (/ time-to-travel (sub1 n))])
    (for/list ([i n])
      (let ([t (+ (pth-min pth) (* i each-delta-t))])
        (pt t ((pth-fx pth) t) ((pth-fy pth) t))))))

;; get-equidistant-points : pth nat[>=2] -> (listof pt)
;; Returns n points that separate the arc described by path into equal lengths
(define (evenly-across-range pth n)
  (let ([fx (pth-fx pth)]
        [fy (pth-fy pth)])
    (let refine-samples ([attempt 1])
      (let* ([numsamples (sub1 (* n (expt 2 attempt)))]
             [sample-width (/ (- (pth-max pth) (pth-min pth)) (sub1 numsamples))]
             [samples 
              (for/list ([n numsamples])
                (let ([t (+ (pth-min pth) (* sample-width n))])
                  (pt t (fx t) (fy t))))]
             [distances (pairwise-distances samples)]
             
             ; estimate total arc length
             [total-len (apply + distances)]
             [segment-len (/ total-len (sub1 n))] ;; TODO: make sure segment-len > tolerance
             )
        (cond 
          [(> (apply max distances) (+ segment-len epsilon))
           (refine-samples (add1 attempt))]
          [else
           (let loop ([pts (cdr samples)]
                      [ds distances]
                      [pts-to-find n]
                      [acc '()]
                      [current-location (car samples)]
                      [to-travel 0]
                      [total-distance-to-travel total-len])
             (cond
               [(= pts-to-find 0) (reverse acc)]
               [(within-tolerance? to-travel)
                (loop pts
                      ds
                      (sub1 pts-to-find)
                      (cons current-location acc)
                      current-location
                      (+ segment-len to-travel) ;; add in the extra under/overshoot distance, so we can make it up
                      total-distance-to-travel)]
               [(< to-travel 0)
                ;; we overshot, our estimate wasn't good enough,
                ;; try the whole thing over again with a larger sample
                ;; size
                (refine-samples (add1 attempt))]
               [else
                (loop (cdr pts)
                      (cdr ds)
                      pts-to-find
                      acc
                      (car pts)
                      (- to-travel (car ds))
                      (- total-distance-to-travel (car ds)))]))])))))

;; path->angle-finder : pth -> number? -> number?
;; determines the angle that an object would be facing if it were travelling
;; across pth from start to finish at time t.
(define (path->angle-finder pth)
  (let ([dfx (derivative (pth-fx pth))]
        [dfy (derivative (pth-fy pth))])
    (λ (t)
      (let* ([dy (dfy t)]
             [dx (dfx t)])
        (if (= dx 0) 
            ((if (> dy 0) + -) (/ pi 2))
            (+ (atan (/ dy dx))
               (if (< dx 0) pi 0)))))))

;; distibute : pth? 
(define (distribute path picters
                    #:divide [get-points-sym 'evenly-across-range]
                    #:rotate [rotate-plain-picts? #f])
  (let* ([get-points (if (eq? get-points-sym 'evenly-across-domain)
                         evenly-across-domain
                         evenly-across-range)]
         [angle (path->angle-finder path)]
         [places (get-points path (length picters))]
         [origin (blank 1)])
    (let loop ([picts picters]
               [pts places]
               [acc origin])
      (cond
        [(null? picts) (panorama acc)]
        [else
         (let* ([pt (car pts)]
                [pict-or-pictfn (car picts)]
                [p (cond
                     [(procedure? pict-or-pictfn)
                      (if (procedure-arity-includes? pict-or-pictfn 2)
                          (pict-or-pictfn (angle (pt-t pt)) (pt-t pt))
                          (pict-or-pictfn (angle (pt-t pt))))]
                     [rotate-plain-picts?
                      (rotate pict-or-pictfn (angle (pt-t pt)))]
                     [else pict-or-pictfn])])
           (loop (cdr picts)
                 (cdr pts)
                 (pin-over acc
                           (pt-x pt)
                           (- (pt-y pt))
                           p)))]))))

;; derivative : (real? -> real?) [real?] -> real? -> real?
;; Returns a function that numerically approximates the derivative of f based
;; on the standard formula
(define (derivative f [h .0001])
  (lambda (x)
    (/ (- (f (+ x h)) (f x))
       h)))