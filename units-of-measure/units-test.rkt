#lang racket

(require rackunit)
(require "units.rkt")

(define-dimension time)
(define-dimension distance)

(define-scale time standard-time (minutes 1) (hours 60))
(define-scale distance english-distance (inches 1) (feet 12) (miles 63360))
(define-scale distance metric-distance (meters 1) (kilometers 1000))
(set-conversion! meters 3.2808399 feet)

;; fake scales for easy-scale-conversion testing
(define-dimension fake)
(define-scale fake foo (foos 1) (kilofoos 1000))
(define-scale fake bar (bars 1) (kilobars 1000))
(set-conversion! foos 10 bars) ; a foo is worth 10 bars

(define feet-per-minute (divide-units feet minutes))
(define feet-per-hour (divide-units feet hours))
(define miles-per-hour (divide-units miles hours))
(define square-feet-per-minute (divide-units (multiply-units feet feet) minutes))
(define square-inches-per-minute (divide-units (multiply-units inches inches) minutes))
(define inches-per-minute (divide-units inches minutes))
(define inches-per-hour (divide-units inches hours))
(define minutes-per-inch (divide-units minutes inches))
(define square-inches (multiply-units inches inches))
(define square-feet (multiply-units feet feet))

;; multiply-units unit tests
(check-equal? 
 (multiply-units feet feet)
 square-feet)
(check-equal? 
 (multiply-units inches inches)
 square-inches)
(check-equal?
 (multiply-units inches feet)
 square-inches)
(check-equal?
 (multiply-units feet inches)
 square-feet)
(check-equal? 
 (multiply-units unitless square-feet-per-minute)
 square-feet-per-minute)
(check-equal? 
 (multiply-units square-feet-per-minute unitless)
 square-feet-per-minute)
(check-equal? 
 (multiply-units square-feet-per-minute (inverse square-feet-per-minute))
 unitless)
(check-equal? 
 (multiply-units square-feet-per-minute hours)
 square-feet)
(check-equal? 
 (multiply-units square-feet-per-minute (inverse hours))
 (divide-units square-feet (multiply-units minutes minutes)))

;; conversion-factor tests
(check-equal?
 (conversion-factor inches feet)
 1/12)
(check-equal?
 (conversion-factor feet inches)
 12)
(check-equal?
 (conversion-factor feet-per-minute inches-per-minute)
 12)
(check-equal?
 (conversion-factor inches-per-minute feet-per-minute)
 1/12)
(check-equal?
 (conversion-factor inches-per-minute inches-per-hour)
 60)
(check-equal?
 (conversion-factor inches-per-minute feet-per-hour)
 60/12)
(check-exn
 (λ (e)
   (and (exn:fail:contract:conversion? e)
        (equal? (exn:fail:contract:conversion-from e) inches)
        (equal? (exn:fail:contract:conversion-to e) minutes)))
 (λ () (conversion-factor inches minutes)))

;; plus tests
(check-equal?
  (plus (inches-per-minute 10) (inches-per-minute 20))
  (inches-per-minute 30))
(check-equal?
  (plus (inches-per-minute 10) (feet-per-minute 1))
  (inches-per-minute 22))
(check-equal?
  (plus (inches-per-minute 10) (feet-per-hour 60))
  (inches-per-minute 22))
(check-equal?
  (plus (inches-per-minute 10) (inches-per-hour -60) (feet-per-hour 60))
  (inches-per-minute 21))

;; times-measures tests
(check-equal?
 (times-measures (inches-per-minute 10) (minutes 20))
 (inches 200))
(check-equal?
 (times-measures 10 20)
 200)
(check-equal?
 (times-measures (inches 10) 2)
 (inches 20))
(check-equal?
 (times-measures 2 (inches 10))
 (inches 20))

;; divide-measures tests
(check-equal?
 (divide-measures (inches 10) (minutes 1))
 (inches-per-minute 10))
(check-equal?
 (divide-measures (inches 10) (minutes 2))
 (inches-per-minute 5))
(check-equal?
 (divide-measures (inches 10) 2)
 (inches 5))
(check-equal?
 (convert 
  (divide-measures (miles 10) (minutes 20))
  miles-per-hour)
 (miles-per-hour 30))

;; implicit scale conversions tests
(check-equal?
 (convert (foos 1) bars)
 (bars 10))
(check-equal?
 (plus (bars 1) (foos 1))
 (bars 11))
(check-equal?
 (convert (divided-by (bars 10) (hours 1)) (divided-by foos hours))
 (divided-by (foos 1) (hours 1)))

(let ([* times] [/ divided-by] [+ plus] [- minus])
  (check-equal?
   (let ([distance (- (miles (* 10 2)) (miles (- 20 10)))]
         [duration (+ (minutes 20) (hours 1/6))])
     (convert (/ distance duration) (/ miles hours)))
   (miles-per-hour 20)))