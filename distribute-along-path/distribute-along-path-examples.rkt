#lang slideshow
(require "distribute-along-path.rkt")

;; ============================================================
;; Examples of using the distribute-along-path library to build
;; interesting effects.

(let* ([rec1 (colorize (filled-rectangle 70 70) "red")]
       [rec2 (colorize (filled-rectangle 70 70) "blue")]
       [cloud (cloud 150 70)]
       [machine (desktop-machine 1)]
       [scene (panorama (pin-over cloud 300 400 machine))])  
  (slide 
   (para "The most basic thing you can do with " (tt "distribute")
         " is distribute picts evenly between two targets. Let's try "
         "putting dots between these two boxes.")
   'alts
   (let ([scene (hc-append 400 rec1 rec2)])
     (list
      (list scene)
      (list (distribute-between rec1 rec2 scene
                                (for/list ([_ 5]) (colorize (disk 10) "purple")))))))
  
  (slide
   (para "Paths don't need to be horizontal ...")
   'alts
   (list (list scene)
         (list (distribute-between cloud machine scene
                                   (for/list ([_ 5]) (arrow 20 0))))))
  
  (slide
   (para "... and you can rotate objects to follow the path.")
   (distribute-between cloud machine scene
                                   (for/list ([_ 5]) (λ (r) (arrow 20 r))))))

(define straight-path (pth (λ (t) t) (λ (t) 0) -150 150))
(define sin-path (pth (λ (t) t) (λ (x) (* 30 (sin (* x (/ pi 150))))) -150 300))
(define spiral (pth (λ (t) (* (+ 50 (* t 2)) (sin t)))
                    (λ (t) (* (+ 50 (* t 2)) (cos t)))
                    0
                    (* pi 5/2)))

(define (string->picters string #:style [style '()] #:size [size 12])
  (map 
   (λ (char)
     (λ (angle) 
       (text (list->string (list char)) style size angle)))
   (string->list string)))

(define picters (string->picters "Abstraction ... it's fun!"))

(define (oriented-fish a)
  (standard-fish 20 20 #:direction (if (> (cos a) 0) 'right 'left)))

(define (s p) (distribute p picters))
(define ((s2 n) p) (distribute p (for/list ([_ n]) (λ (a) (arrow 10 a)))))
(define ((s3 n) p) (distribute p (for/list ([_ n]) (arrow 10 0))))
(define ((s4 n) p) (distribute p (for/list ([_ n]) (arrow 10 0)) #:rotate #t))
(define ((s5 n) p) (distribute p (for/list ([_ n]) oriented-fish)))
(define ((s6 n) p) (distribute p (for/list ([_ n]) (standard-fish 20 20)) #:rotate #t))

(for-each (λ (s)
            (slide #:layout 'center
             (s straight-path)
             (blank 20)
             (s spiral)
             (blank 20)
             (s sin-path)))
          (list s (s2 20) (s3 20) (s4 20) (s5 15) (s6 15)))

(define 1/xpath (pth (λ (t) t) (λ (t) (/ 200 t)) 1 200))

(slide #:layout 'center
       (distribute 1/xpath (build-list 20 (λ (_) (λ (a) (arrow 10 a)))))
       (blank 20)
       (distribute 1/xpath (build-list 20 (λ (_) (λ (a) (arrow 10 a))))
                   #:divide 'evenly-across-domain))

(slide #:layout 'center 
       (distribute
        (pth (λ (t) 0) (λ (t) (* t -15)) 0 20)
        (build-list 20 (λ (_) (λ (a) (arrow 15 a))))))

(require (prefix-in srfi13: (lib "13.ss" "srfi")))
(define (poem-para . strs)
  (srfi13:string-join strs " / "))

(define Casabianca-title
  (string-append "  CASABIANCA  "
                 "Elizabeth Bishop (1911-1979)"))
(define Casabianca
  (string-append
   (poem-para
    "Love's the boy stood on the burning deck"
    "trying to recite \"The boy stood on"
    "the burning deck.\" Love's the son"
    "stood stammering elocution"
    "while the poor ship in flames went down.")
   "  "
   (poem-para
    "Love's the obstinate boy, the ship,"
    "even the swimming sailors, who"
    "would like a schoolroom platform, too,"
    "or an excuse to stay"
    "on deck. And love's the burning boy.  ")))
(define casabianca-picters 
  (append
   (string->picters Casabianca-title #:style '(bold) #:size 18)
   (string->picters Casabianca)))

(define neato-path
  (pth (λ (t) (* 23 (- (* 10 (sin t)) (* 7 (sin (* 3/2 t)))))) 
       (λ (t) (* 23 (- (* 10 (cos t)) (* 7 (cos (* 3/2 t))))))
       0.1
       (* pi 4)))

(slide #:layout 'center (distribute neato-path casabianca-picters #:divide 'evenly-across-domain))
(slide #:layout 'center (distribute neato-path casabianca-picters #:divide 'evenly-across-range))

(define neato2
  (pth (λ (t) (* 370 (cos (* 3 t))))
       (λ (t) (* 340 (sin (* 5 t))))
       0
       (* 2 pi)))

(slide #:layout 'center (distribute neato2 casabianca-picters))

(define steps 300)
#;(let loop ([i 0])
  (cond 
    [(= i steps) (void)]
    [else
     (begin
       (slide #:layout 'center 
              (distribute 
               (pth (λ (t) (* 200 (cos (* 3 t))))
                    (λ (t) (* 200 (sin (* 5 t))))
                    0
                    (* 2 pi))
               (for/list ([n steps]) 
                 (if (= n i) oriented-fish (λ (a) (blank 10))))))
       (loop (add1 i)))]))