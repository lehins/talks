# Talks

Material presented at various events

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Talks](#talks)
    - [2020 - HaskellerZ - Color in Haskell](#2020---haskellerz---color-in-haskell)
        - [Abstract](#abstract)
    - [2019 - Monadic Party - Haskell arrays with Massiv](#2019---monadic-party---haskell-arrays-with-massiv)
        - [Abstract](#abstract-1)
    - [2019 - FP Complete webinar - RIO, a standard library for Haskell](#2019---fp-complete-webinar---rio-a-standard-library-for-haskell)
        - [Abstract](#abstract-2)
    - [2018 - Monadic Warsaw #14 - Haskell arrays that are easy and fast](#2018---monadic-warsaw-14---haskell-arrays-that-are-easy-and-fast)
        - [Abstract](#abstract-3)

<!-- markdown-toc end -->


## 2020 - HaskellerZ - Color in Haskell

* Talk page is on [meetup.com](https://www.meetup.com/HaskellerZ/events/271009754/)
* Related materials are in this repo: [2020-HaskellerZ](2020-HaskellerZ)
* Recording of the talk is on [youtube.com](https://youtu.be/5Y9mcR00gpo)

### Abstract

This talk targets any beginner to advanced level developer who would like to understand
ways to represent and manipulate colors programmatically in Haskell with the
[Color](https://github.com/lehins/Color) library

The goal is to convey the meaning of what a color space is and shatter the common
confusion with the color model. Discusses concepts involved in defining various color
spaces and conversion between them. There will be a tutorial on how to write correct and
efficient code that performs these conversions without the need to understand the math
behind it. Lastly we'll see how to apply the knowledge learnt to convert some images.

## 2019 - Monadic Party - Haskell arrays with Massiv

* Talk page is on [2019.monadic.party](https://2019.monadic.party/#talks)
* Presented materials are:

  * in this repo: [2019-Monadic-Party](2019-Monadic-Party)
  * and in a separate [RockAnts](https://github.com/lehins/RockAnts) repository.

* Recordings of this workshop is on youtube.com:

  * [Haskell Arrays with Massiv 1/6](https://www.youtube.com/watch?v=euEacUD6jQQ)
  * [Haskell Arrays with Massiv 2/6](https://www.youtube.com/watch?v=WTeaDUOrbaw)
  * [Haskell Arrays with Massiv 3/6](https://www.youtube.com/watch?v=vTbpeugaucI)
  * [Haskell Arrays with Massiv 4/6](https://www.youtube.com/watch?v=S9rGyjmjpos)
  * [Haskell Arrays with Massiv 5/6](https://www.youtube.com/watch?v=yyXaR8MXUkI)
  * [Haskell Arrays with Massiv 6/6](https://www.youtube.com/watch?v=CdOMsREWJeg)

### Abstract

If you are not used to the pure functional paradigm, then dealing with arrays in Haskell
can be very unintuitive, the code you write can become inefficient, especially if you
choose the wrong library or an incorrect data structure for the job.

Throughout the series of talks and workshops I will cover some concepts about native
arrays that every Haskell programmer should understand as well as provide concrete
examples on how to deal with multidimensional arrays using the `massiv` library. First
lectures will start with basic topics, such as memory representations and how to properly
handle mutation. Then we will transition into more advanced notions of fusion, stencils
and other ways of avoiding unnecessary computation, while taking advantage of automatic
parallelization. Besides looking at many simple examples we will have hands on experience
developing a more complex application that will reinforce the understanding of concepts
introduced earlier.

## 2019 - FP Complete webinar - RIO, a standard library for Haskell

* Talk page is on [fpcomplete.com](https://www.fpcomplete.com/blog/rio-standard-library-for-haskell/)
* Slides are in this repo: [2018-FPCo-Webinar](2019-FPCo-Webinar/slides-rio-webinar.md)
* Recording of the talk is on [youtube.com](https://www.youtube.com/watch?v=gu0ZCqQe3BY)
* Material presented:

  * [rio](https://github.com/commercialhaskell/rio) library
  * [haskell-webshell](https://github.com/lehins/haskell-webshell) - shell as a webapp

### Abstract

In this month's webinar, Alexey Kuleshevich demonstrated just how easy it is to get
started with RIO, the standard library for Haskell. As a recap, RIO is not only a library
but is a collection of solutions to some of the most common problems in the Haskell
ecosystem as well as a description of the best practices and design patterns. It also
introduces the RIO monad, which promotes a drastic simplification over the common approach
of an endless stack of transformers.

## 2018 - Monadic Warsaw #14 - Haskell arrays that are easy and fast

* Talk page is on [meetup.com](https://www.meetup.com/Monadic-Warsaw/events/249543097/)
* Slides are in this repo: [2018-Monadic-Warsaw](2018-Monadic-Warsaw/2018-MonadicWarsaw.pdf)
* Recording of the talk is on [youtube.com](https://www.youtube.com/watch?v=AAx2a0bUsxA)

### Abstract

This talk will be about manipulation of multidimensional arrays in Haskell using freshly
baked library called massiv. No prior exposure to arrays in Haskell will be required,
understanding of what an array is and familiarity with basic functional programming should
be sufficient.

First part of the talk will be an overview of the library and some of the core concepts:
in memory data representation, array indexing and manual fusion.

Further will follow some examples of array processing mechanisms such as folding,
extracting, slicing etc. after which we'll dive into some more advanced features, in
particular stencil computation, parallelization and overview of the scheduler.

Performance and ease of use are the two main goals of the library, therefore expect plenty
of usage examples, benchmark results and some comparison to existing libraries such as
Vector, Repa and Accelerate.

Time permitting, we might look into massiv application in Image Processing.
