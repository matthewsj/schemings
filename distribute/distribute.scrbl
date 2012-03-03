#lang scribble/manual

@(require planet/scribble
          planet/version
          (for-label racket)
          (for-label (this-package-in distribute)))

@title{@bold{distribute}: Distributing picts across paths in Slideshow}

Jacob Matthews

@tt{jacobm at gmail}

The @tt{distribute} library provides a simple way of distributing a series of picts evenly across a path in a Slideshow presentation.
The path can be a line between two existing picts or arbitrary points, or it can be a general curve.

This document contains API reference. For an introduction, 
@racket[(require (planet #,(this-package-version-symbol distribute-examples)))]
for a slideshow that demonstrates this package's features.

@section{API Reference}
@defmodule/this-package[distribute]
@defproc[(distribute-between (origin pict?) (destination pict?) (scene pict?) (picts (listof (or/c pict? (-> real? pict?)))) 
                             (#:origin-locator find-origin-pt (-> pict? pict? (values real? real?)) rc-find)
                             (#:destination-locator find-destination-pt (-> pict? pict? (values real? real?)) lc-find)
                             (#:rotate rotate? boolean? #f))
         pict?]{
Returns a pict whose bounding box is the same as @racket[scene], but with the picts described in @racket[picts]
distributed evenly between the points designated by @racket[(find-origin-pt scene origin)] and @racket[(find-destination-pt scene destination)].

Values for which @racket[pict?] returns true will be placed in an appropriate position along the path from
@racket[origin] to @racket[destination]. If @racket[#:rotate] is true, then each pict will be rotated to
point in the direction that an object in its position would be pointing if it were travelling from 
@racket[origin] to @racket[destination]. Functions provided in @racket[picts] will be provided an appropriate
angle and expected to produce an appropriate pict for the given rotation. (These functions are provided a
rotation regardless of whether @racket[#:rotate] is set to true or not.)}

@defstruct*[pth ((fx (-> real? real?))
                 (fy (-> real? real?))
                 (min real?)
                 (max real?))]{
Represents a path described by two functions from some abstract @racket[t] value to @racket[x] and @racket[y] 
coordinates using the functions @racket[fx] and @racket[fy], respectively, where @racket[t] ranges from
@racket[min] to @racket[max]. The @racket[x] and @racket[y] coordinates determine the overall size and layout
of the path, but not an absolute position on the screen (which is determined by picts produced by the path
using the same tools used to lay out all picts).}
               
@defproc[(path-between (origin pict?) (destination pict?) (scene pict?)
                       (#:origin-locator find-origin-pt (-> pict? pict? (values real? real?)) rc-find)
                       (#:destination-locator find-destination-pt (-> pict? pict? (values real? real?)) lc-find))
         pth?]{
Returns the path between @racket[(find-origin-pt scene origin)] and @racket[(find-destination-pt scene destination)].
}

@defproc[(distribute (path pth?) (picts (listof (or/c pict? (-> real? pict?))))
                     (#:divide divide (symbols 'evenly-across-domain 'evenly-across-range) 'evenly-across-range)
                     (#:rotate rotate? boolean? #f))
         pict?]{
Produces a pict of each pict in @racket[picts] distributed across the path described by @racket[path]. The supplied picts
or functions are rotated as described in the documentation for @racket[distribute-between], above. If
@racket[#:divide] is set to @racket['evenly-across-domain], then picts are laid out by dividing @racket[t] in equal increments
from @racket[(pth-min path)] to @racket[(pth-max path)]; otherwise picts are placed equal arc-lengths apart across the path.}
