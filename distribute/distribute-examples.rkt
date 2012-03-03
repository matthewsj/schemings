#lang slideshow
(require "distribute.rkt")
(require (prefix-in srfi13: (lib "13.ss" "srfi")))
(require slideshow/code)

;; ============================================================
;; Slideshow tutorial of using distribute-along-path.

(define (typeset-string str)
  (let ((p (open-input-string str)))
    (port-count-lines! p)
    (typeset-code (read-syntax 'string p))))

(slide
 (titlet "distribute.plt")
 (blank 20)
 (t "Jacob Matthews"))

(let* ([red-rectangle (colorize (filled-rectangle 70 70) "red")]
       [blue-rectangle (colorize (filled-rectangle 70 70) "blue")]
       [cloud (cloud 150 70)]
       [machine (desktop-machine 1)]
       [scene1 (hc-append 400 red-rectangle blue-rectangle)]
       [scene2 (panorama (pin-over cloud 300 400 machine))]) 
  (slide 
   (para "The most basic thing you can do with " (tt "distribute")
         " is distribute picts evenly between two targets.")
   (typeset-string
"(distribute-between 
 red-rectangle blue-rectangle scene
 (for/list ([_ 5]) (colorize (disk 10) \"purple\")))")
   (blank 10)
   (distribute-between 
    red-rectangle blue-rectangle scene1
    (for/list ([_ 5]) (colorize (disk 10) "purple"))))
  
  (slide
   (para "Paths don't need to be horizontal ...")
   (distribute-between cloud machine scene2
                       (for/list ([_ 5]) (arrow 20 0))))
  
  (slide
   (para "... and you can rotate objects to follow the path.")
   (distribute-between cloud machine scene2
                       (for/list ([_ 5]) (λ (r) (arrow 20 r))))))

(define straight-path (pth (λ (t) t) (λ (t) 0) -150 150))
(define sin-path (pth (λ (t) t) (λ (x) (* 30 (sin (* x (/ pi 150))))) -150 300))
(define spiral (pth (λ (t) (* (+ 50 (* t 2)) (sin t)))
                    (λ (t) (* (+ 50 (* t 2)) (cos t)))
                    0
                    (* pi 5/2)))

(slide (para "Using " (typeset-code #'pth) 
             " you can also distribute objects across arbitrary paths. "
             (typeset-code #'pth) " takes functions from " (typeset-code #'t) 
             " to x and y coordinates, along with starting and ending values of "
             (typeset-code #'t) " and produces a path along which you can distribute picts "
             "using " (typeset-code #'distribute) ".")
       'next
       (typeset-string 
"(distribute (pth (λ (t) t) (λ (t) 0) -200 200)
            (for/list ([_ 10]) (arrow 30 0)))")
       (blank 10)
       (distribute (pth (λ (t) t) (λ (t) 0) -200 200)
                   (for/list ([_ 10]) (arrow 30 0))))

(let ()
  (struct slide-contents (text code pict))
  (define slides
    (list
     (slide-contents
      (para "The path you create doesn't need to be a straight line ...")
      (typeset-string 
"(distribute 
 (pth (λ (t) t) 
      (λ (x) (* 30 (sin (* x (/ pi 150)))))
      -150 300)
 (for/list ([_ 10]) (arrow 30 0)))")
      (distribute (pth (λ (t) t) (λ (x) (* 30 (sin (* x (/ pi 150))))) -150 300)
                  (for/list ([_ 10]) (arrow 30 0))))
     (slide-contents
      (para "... and you can easily rotate objects to point in the "
            "direction of the path by passing " 
            (typeset-code #'#:rotate) " " (typeset-code #'true)
            " to " (typeset-code #'distribute) ".")
      (typeset-string
"(distribute 
 (pth (λ (t) t) 
      (λ (x) (* 30 (sin (* x (/ pi 150)))))
      -150 300)
 (for/list ([_ 10]) (arrow 30 0))
 #:rotate true)")
      (distribute 
       (pth (λ (t) t) 
            (λ (x) (* 30 (sin (* x (/ pi 150)))))
            -150 300)
       (for/list ([_ 10]) (arrow 30 0))
       #:rotate true))
     (slide-contents
      (para "You can perform an arbitrary computation to go from expected rotation to a pict "
            "by supplying a function from angle to pict instead of a pict. You might want to "
            "use this with functions like " (typeset-code #'text) " that can produce better "
            "picts if you supply a rotation.")
      (typeset-string
"(distribute 
 (pth (λ (t) t) 
      (λ (x) (* 30 (sin (* x (/ pi 150)))))
      -150 300)
 (for/list ([_ 10]) (λ (θ) (text \"wave\" '() 12 θ))))")
      (distribute 
       (pth (λ (t) t) 
            (λ (x) (* 30 (sin (* x (/ pi 150)))))
            -150 300)
       (for/list ([_ 10]) (λ (θ) (text "wave" '() 12 θ)))))))
    (for ([item slides])
      (define ((maybe-show selector) sc) (if (eq? sc item) (selector sc) (ghost (selector sc))))
      (slide
       (apply ct-superimpose (map (maybe-show slide-contents-text) slides))
       (apply ltl-superimpose (map (maybe-show slide-contents-code) slides))
       (blank 10)
       (apply ct-superimpose (map (maybe-show slide-contents-pict) slides)))))

(let ()
  (struct slide-contents (text code pict))
  (define slides 
    (list
     (slide-contents (para "By default, " (typeset-code #'distribute) " distributes picts "
                           "evenly across the " (it "range") " of the function you supplied "
                           "-- that is, it divides the path up into equal lengths. To see this in action, "
                           "let's distribute dots across the path described by x=t, y=200/t from t=1 to 200: ")
                     (typeset-string
"(distribute 
 (pth (λ (t) t) (λ (t) (/ 200 t)) 1 200)
 (for/list ([_ 20]) (circle 10)))")
                     (distribute 
                      (pth (λ (t) t) (λ (t) (/ 200 t)) 1 200)
                      (for/list ([_ 20]) (circle 10))))
     (slide-contents (para "We could also distribute evenly across the " (it "domain") " of t, "
                           "for instance to plot the position of accelerating objects over equal time "
                           "ranges. To do that, provide the keyword argument " 
                           (typeset-code #'#:divide) " " (typeset-code #''evenly-across-domain) " to "
                           (typeset-code #'distribute) ":")
                     (typeset-string 
"(distribute 
 (pth (λ (t) t) (λ (t) (/ 200 t)) 1 200)
 (for/list ([_ 20]) (circle 10))
 #:divide 'evenly-across-domain)")
                     (distribute 
                      (pth (λ (t) t) (λ (t) (/ 200 t)) 1 200)
                      (for/list ([_ 20]) (circle 10))
                      #:divide 'evenly-across-domain))))
  (for ([item slides])
    (define ((maybe-show selector) sc) (if (eq? sc item) (selector sc) (ghost (selector sc))))
    (slide
     (apply ct-superimpose (map (maybe-show slide-contents-text) slides))
     (apply ltl-superimpose (map (maybe-show slide-contents-code) slides))
     (blank 10)
     (apply ct-superimpose (map (maybe-show slide-contents-pict) slides)))))
             
(define (string->picters string #:style [style '()] #:size [size 12])
  (map 
   (λ (char)
     (λ (angle) 
       (text (list->string (list char)) style size angle)))
   (string->list string)))

(define (oriented-fish a)
  (standard-fish 20 20 #:direction (if (> (cos a) 0) 'right 'left)))

(let ()
  (define picters (string->picters "Abstraction ... it's fun!"))
  (define (s p) (distribute p picters))
  (define ((s2 n) p) (distribute p (for/list ([_ n]) (λ (a) (arrow 10 a)))))
  (define ((s3 n) p) (distribute p (for/list ([_ n]) (arrow 10 0))))
  (define ((s4 n) p) (distribute p (for/list ([_ n]) oriented-fish)))
  (define ((s5 n) p) (distribute p (for/list ([_ n]) (standard-fish 20 20)) #:rotate #t))
  (slide #:layout 'center
         (para "Since " (typeset-code #'distribute) " lets you specify paths and objects "
               "to distribute separately, you can reuse either to lay out the same picts "
               "in different ways, or different picts across the same paths.")
         (blank 10)
         'alts
         (for/list ([fn-for-path (list s (s2 20) (s3 20) (s4 15) (s5 15))])
           (list
            (fn-for-path straight-path)
            (blank 10)
            (fn-for-path spiral)
            (blank 10)
            (fn-for-path sin-path)))))

(let ()
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
  
  (define interesting-path
    (pth (λ (t) (* 23 (- (* 10 (sin t)) (* 7 (sin (* 3/2 t)))))) 
         (λ (t) (* 23 (- (* 10 (cos t)) (* 7 (cos (* 3/2 t))))))
         0.1
         (* pi 4)))
  
  (define interesting-path-2
    (pth (λ (t) (* 370 (cos (* 3 t))))
         (λ (t) (* 340 (sin (* 5 t))))
         0
         (* 2 pi)))
  
  (slide #:layout 'center (distribute interesting-path casabianca-picters))
  (slide #:layout 'center (distribute interesting-path-2 casabianca-picters)))

(let ([steps 100]
      [path (pth (λ (t) (* 200 (cos (* 3 t))))
                 (λ (t) (* 200 (sin (* 5 t))))
                 0
                 (* 2 pi))]
      [thanks (hbl-append (t "Thanks for using ") (tt "distribute.plt") (t "!"))])
  (for ([i steps])
    (slide #:layout 'center 
           (ghost thanks)
           (distribute 
            path
            (for/list ([n steps])
              (if (= n i) oriented-fish (λ (a) (ghost (oriented-fish a))))))))
  (slide #:layout 'center 
         thanks
         (distribute 
          path
          (for/list ([n 200]) oriented-fish))))