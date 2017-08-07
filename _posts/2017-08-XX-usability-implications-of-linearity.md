---
layout: post
title: Usability implications of linearity
---

- Stream functions work both for linear and non-linear monads, how could we make
  that a thing without duplicating all functions?

- Even in the case of linear monads, we may have some notion of `interruptible`
  monad where we could call `take`. What would that notion be?  It is important
  to acknowledge this limitation. And we can think of a solution when all is
  well layed-out

- you will probably not use _only_ linearity when developing something with the
  extension, and mixed monad classes in a module is paain. Like, you have to
  figure out which parts will be easiest/simplest to express with qualified
  infix monadic operators, since you will have to nuke the do-notation for one
  of them because `-XRebindableSyntax` is only full-module.  (then you realise
  "wow, this wasn't as easy as I thought", switch strategies and have to
  re-write the code haha).

- Like the record-hack from hell? :balloon:

- Oooor, you could refactor what is logically one module into two only to get
  screwed by recursive dependencies, get confused by your own codebase, and
  bullied by your code reviewers
