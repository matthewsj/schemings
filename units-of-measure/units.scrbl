#lang scribble/manual
@(require "units.rkt")
@(require scribble/eval)

@title{Units of Measure}

@tt{units-of-measure.plt} is a library of convenient utilities for working with units
of measure (e.g. feet, liters, grams). It provides two main features: automatic 
unit-compatibility checking, and conversions (both automatic and manaul) for compatible
units.

@section{Introduction by example}

@(define evaluator (make-base-eval))
The units of measure library divides the world into a few related concepts:
@itemlist[
          @item{@italic{dimensions} (things that can be measured,
like time, distance, or mass),}
          @item{@italic{scales} (particular ways of dividing up a dimension, like
the Imperial units of distance,}
          @item{@italic{basic units} (a single demarcation in the unit scale, like
feet or pounds),}
          @item{@italic{derived units} (combinations of basic units, across the same or different dimensions,
like meters per second squared), and}
          @item{@italic{measures} (quantities of a particular unit).}]

The best way to understand these concepts is to see them in action.

@interaction[
  #:eval evaluator
  (eval:alts (require (planet jacobm/units-of-measure:1)) (require "units.rkt"))
  (define-dimension distance)
  (define-dimension time)
  (define-scale distance english-distance (inches 1) (feet 12) (miles 63360))
  (define-scale time standard-time (minutes 1) (hours 60))

  (define-values (+ * - /) (values plus times minus divided-by))
  (let ([distance-travelled (miles 20)]
        [time-taken (minutes 20)]
        [miles-per-hour (/ miles hours)])
    (convert (/ distance-travelled time-taken) miles-per-hour))]
             
The units of measure library also performs conversions to make math among compatible
units easy, while catching mistakes:
@interaction[
    #:eval evaluator
    (+ (miles 10) (miles 20))
    (+ (miles 10) (inches 20))
    (+ (miles 10) (hours 20))]

When converting units, the library consistently applies a "left-unit wins" rule, which
means that unit addition is not exactly commutative:
@interaction[
    #:eval evaluator
    (+ (inches 12) (feet 1))
    (+ (feet 1) (inches 12))]

Another interesting case of autoconversion occurs during multiplication and division.
All occurences of a single measurement scale are automatically converted to the same
base unit. For instance,
@interaction[
    #:eval evaluator
    (* (feet 10) (inches 6))]

Sometimes you want to convert among units that measure the same dimension but use different
scales --- for instance when mixing English and metric units. To do so, use
@racket[set-conversion!] to configure a conversion ratio from an arbitrary unit of one scale to
an arbitrary unit in the other, and the library will figure out the rest.

@interaction[
    #:eval evaluator
    (define-scale distance metric-distance (meters 1) (kilometers 1000))
    (set-conversion! meters 3.2808399 feet)
    (+ (kilometers 5) (miles 2))]

Calculations that produce unitless results automatically convert into basic numbers:
@interaction[
    #:eval evaluator
    (define miles-per-hour (/ miles hours))
    (* (miles-per-hour 10) ((inverse miles-per-hour) 20))]

@(close-eval evaluator)