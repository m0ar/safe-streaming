Safe streaming with linear types
================================

This is a fork of the original [streaming](github.com/michaelt/streaming)
library, modified for increased type safety through utilisation of **linear
types**! I have been working on this in tight collaboration with software
innovation lab [Tweag I/O](https://www.tweag.io/) over the summer, and have
been able to do this thanks to the
[Summer of Haskell](https://summer.haskell.org/) stipend. I have also documented
interesting happenings throughout the project in a
[blog](https://m0ar.github.io/safe-streaming/), check it (and corresponding
Reddit discussions) out!


This project is made possible by a new linear types extension to GHC that is
currently in development by Tweag I/O with friends. For the curious, it can be
found [here](https://github.com/tweag/ghc/tree/linear-types). I have written
instructions in the README of that branch on how you can get this running
(today!) either by compiling this GHC version yourself, or using a
precompiled build from a Docker image which integrates nicely with Stack.
It also documents how you can get started with writing linearly typed
programs, debugging tips, and some of the peculiarities that we have found so
far. Remember this is still work in progress so some things are a bit weird,
but it already works pretty well. This extension is _required_ to build this
project, since it uses novel type system features not present in vanilla GHC.



### What's wrong with `streaming`?
Well, these streams are _effectful_, meaning that they can perform arbitrary
monadic actions to conjure their elements. When you ask a stream for its next
value, it will return you this together with a reference to the rest of the
stream. The old stream reference (that you just used) should now be considered
dead; only the new one is to be used. This is because if you _again_ pull from
the first stream reference, it will _repeat_ whatever monadic action that was
performed to produce that last element! This is capable of resulting both in
arbitrarily weird and unexpected results, as well as runtime errors. In my
[project blog](https://m0ar.github.io/safe-streaming/2017/06/19/linear-types-101.html)
I have documented this issue in better detail.


In adition to this, when we stream from a file handle, something requiring a
lock, or any similar "borrow-and-return"-abstraction, it would be nice to have
the type system help you with this responsibility of cleanup. This would mean
that errors of the classical use-after-free and double-free kind would
not even _compile_.


As you might have figured out, these are both problems of **linearity**, and
can be solved with linear types. A value of linear type _has to be used
exactly once_. So for the first problem, if we return linear streams, the
stream variables _can't_ be re-used. For the other, we can stick a function
that closes your filehandle (or whatever you need to do) as a linear value at
the end of the stream. Now the program will not compile if that is not called
in your code. Both of these solutions give you compile time guarantees of things
that you before had to keep track of yourself, which is super neat!



### So what am I doing?
The main point of this project is to see if I can use the linear type system
to solve these problems with `streaming`, and how it affects the semantics of
the library. The [internals](https://github.com/m0ar/safe-streaming/blob/master/src/Streaming/Internal.hs)
contains the functor-general library "backend", and the
[prelude](https://github.com/m0ar/safe-streaming/blob/master/src/Streaming/Prelude.hs)
consists of more specialised functions that mimics the behaviour of the `list`
API as well as other common streaming models like `pipes`, `condutit` and
`io-streams`.



### List of interesting parts to check out
If you aren't in the mood to dig through 4K SLOC and _guess_ where I have hid
other interesting things, here is a list of noteworthy stuff that has come
from this project:

* Mini crash course in
  [streaming](https://m0ar.github.io/safe-streaming/2017/06/13/what-are-streams-anyway.html)
  and [linear types and application in streaming](https://m0ar.github.io/safe-streaming/2017/06/19/linear-types-101.html)

* A linear `Monad` class, together with meshing `Functor` and `Applicative`: [code](https://github.com/m0ar/safe-streaming/blob/master/src/Control/Monad/LMonad.hs), [explanatory blogpost](https://m0ar.github.io/safe-streaming/2017/07/20/homegrown-linear-monads.html)

* A linear State monad which can be parametrised with mixed-linearity state
  types: [code](https://github.com/m0ar/safe-streaming/blob/master/src/Control/Monad/State/LState.hs)
  *future blog post*

* A less-than-extensive [test suite](https://github.com/m0ar/safe-streaming/blob/master/test/Spec.hs)
  that at least provides a light demo on the linear state monad and a safe
  implementation of the main issues.

* This is not my work, but the [underlying paper](https://github.com/tweag/linear-types/releases/download/v2.0/hlt.pdf)
  for the linear types extension to GHC is an interesting read.
