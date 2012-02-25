#lang scribble/manual
@(require "units.rkt")
@(require scribble/eval)

@title{Units of Measure}

@tt{units-of-measure.plt} is a library of convenient utilities for working with units
of measure (e.g. feet, liters, grams). It provides three main features:

automatic unit-compatibility checking and automatic unit conversions for compatible
units.

@section{Introduction by example}

@(define evaluator (make-base-eval))
Here are some examples of units of measure in action. This example demonstrates how
to define groups of units, declare measurements in those units, perform calculations
involving measures, create derived units, and convert measures among compatible units.
@interaction[
  #:eval evaluator
  (eval:alts (require (planet jacobm/units-of-measure:1)) (require "units.rkt"))
  (define-units-of-measure time (minutes 1) (hours 60))
  (define-units-of-measure distance (inches 1) (feet 12) (miles 63360))
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

Calculations that produce unitless results automatically convert into basic numbers:
@interaction[
    #:eval evaluator
    (define miles-per-hour (/ miles hours))
    (* (miles-per-hour 10) ((inverse miles-per-hour) 20))]


@(close-eval evaluator)