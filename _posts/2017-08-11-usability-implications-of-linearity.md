---
layout: post
title: Usability issues with linear Haskell
---

Working with linearly typed Haskell has been really interesting, and I have
leveraged a [linear monad](https://m0ar.github.io/safe-streaming/2017/07/20/homegrown-linear-monads.html)
for much of my endeavours. This has been neat in combination with
`-XRebindableSyntax` to get do-notation to plug-and-play, but not everything
has been a smooth experience. In this post I will discuss a few of the
annoying usability issues that have surfaced when working with linear types in a
practical setting.


## Broken flow control
There are some issues with flow control when using the linear extension.
Albeit a temporary problem, there is no linear `case` and `let` implemented
yet. This means that even if you thread your things linearly through these
constructs, they will lead to errors because the linearity checker sees you
passing a linear variable to an unrestricted context:

{% highlight haskell %}
λ> let f :: a ⊸ a; f x = let y = x in y

<interactive>:28:19: error:
    • Couldn't match expected weight ‘1’ of variable ‘x’ with actual weight ‘ω’
    • In an equation for ‘f’: f x = let y = x in y


λ> data B = T Int | F Int
λ> let f :: B ⊸ Int; f b = case b of T i -> i; F i -> i

<interactive>:3:21: error:
    • Couldn't match expected weight '1' of variable 'b' with actual weight 'ω'
    • In an equation for 'f':
          f b
            = case b of
                T i -> i
                F i -> i
{% endhighlight%}

Both these examples will work eventually, and linear versions of both `case`
and `let` will indeed be implemented later. For now, one can work around these
hurdles through adding `where`-functions and doing the `case` logic through
pattern matching there. There are a few silly constraints like that, but
also real problems so let's take a look at a few of those.


## Mixing monad classes
So, `-XRebindableSyntax` made using a redefined monad a real hoot. Until you
need to mix different kinds of monads in the same module... Then you run into
trouble, because the `do`-notation can only be bound to the functions of _one_
of the monad classes and the others need to use qualified infix monadic
operators. This could be a common issue, because you possibly do not care
about linearity _everywhere_, but only in parts of your code. When
`do`-notation only works for one of your monads, you have a few unsatisfactory
choices like:

* Figure out which of your monads you need to write the simplest code for,
  then write that without `do`-notation using qualified infix monadic
  operators in a sea of lambdas. This only to later realise that it got more
  complex than you anticipated, so you change your mind and have to rewrite
  _that_ code in `do`-notation and change the _other_ code to qualified infix
  riddled spaghetti instead.

* Split up a logically sound module into two separate ones, one for linear
  and one for unrestricted code. Then you get screwed over by recursive
  dependencies, confused by your own code base and/or bullied in code review.


### A solution approach
It should be possible to solve this issue by creating a sort of "monad umbrella",
allowing different multiplicity monads to share a common interface. This could
then be used for functions that want to support all of them. I have been
trying **hard** (with excellent help from the guys at Tweag) to write a proper
implementation that the type checker accepts, using a plethora of different
approaches, but in vain. Anyway, this sketch should show you the idea:


{% highlight haskell %}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Control.Monad as M
import qualified Control.Monad.LMonad as L

data Multiplicity = One | Omega

-- A TF associating the proper
-- arrow to each multiplicity
type family Arrow p a b where
  Arrow One   a b = a ⊸ b
  Arrow Omega a b = a -> b

-- A Monad umbrella class with arrows parameterised
-- by the underlying monad multiplicity
class MonadWithWeight (p :: Multiplicity) m where
  -- a -> m a
  return :: Arrow p a (m a)

  -- m a -> (a -> m b) -> m b
  (>>=) :: Arrow p (m a) (
             Arrow p (Arrow a (m b))
               m b)

instance Monad m => MonadWithWeight 'Omega where
  return = M.return
  (>>=) = (M.>>=)

instance LMonad m => MonadWithWeight 'One where
  return = L.return
  (>>=) = (L.>>=)
{% endhighlight%}

If we made something like this, we could bind the `do`-notation to the
functions provided by `MonadWithWeight` and automagically get the correct
implementation depending on which monad we are currently typed with! Cool.
Though, this does not actually work, which could be from fundamental limits in
the compiler. It is a bit early to speculate but we did it anyway; our best
guess is that the type of `>>=` is inferred in isolation, and rebindable-`do`
requires it to have a function type. So even _if_ GHC unfolds the type family
early enough, `>>=` doesn't in isolation have a specified multiplicity and
hence not yet a decided function arrow.


### A temporary workaround
While the above solution would definitely be better, there _is_ actually a
working solution, but it's not very pretty: pack your favourite monad in a
record and take it with you! Unpack it and override `do`-notation locally,
both practical _and_ fun:

{% highlight haskell %}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}

data LinearMonadOverloader m a b = MO
  { (>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b
  , (>>) :: m () ⊸ m a ⊸ m a
  , return :: a ⊸ m a
  }

linearMonad :: LMonad m =>
  LinearMonadOverloader m a b
linearMonad = MO
  { (>>=) = (L.>>=)
  , (>>)  = (L.>>)
  , return = L.return
  }

-- In another module somewhere, import your
-- function and shadow the monadic operators
myLinearInc :: LMonad m => m Int ⊸ m Int
myLinearInc n = do
  i <- n
  return $ i + 1
  where
    MO { .. } = linearMonad
{% endhighlight%}

The reason this works is that we don't pollute the global scope with different
monadic operators, but hide them in a function returning a record type. This
allows us to import that function globally and unpack them from the record
locally where we need them, shadowing the operators only in the scope of that
`where`-clause!

It is worth noting that this is not specific to linear things but rather the
`-XRebindableSyntax` extension overall, possibly contributing to its seemed
unpopularity. This might be a reason for indexed and relative monads not
seeing so much use either, since the handling of syntax gets so clunky.


## Multiplicity polymorphism for monads
How can we get functions to work with both linear and non-linear abstractions?
To solve the issues with repeatable effects in the `streaming` library, I
redefined it to work with the linear monad. This is great and all, but now the
code _only_ supports linear monads and no longer the good ol' unrestricted
monad. It would be practical to be able to write functions on monads with
different multiplicity constraints, otherwise we will have to write lots of
duplicate code to support the different monads! Unfortunately, general
multiplicity polymorphism requires a lot more work, and will likely not happen
before the monomorphic linear types are merged.


## If-Then-Else
The arguaby nastiest issue is related to the `if-then-else` construct, and is
much worse than `case` and `let`. The core syntax _is_ implemented linearly,
but clashes with `-XRebindableSyntax`; as soon as this extension is activated,
the internal `if-then-else` goes out of scope and you have to provide an
implementation of `ifThenElse :: a -> b -> c -> d` yourself. As you can see,
`-XRebindableSyntax` _assumes_ this is the type we need, an assumption not
compatible with linear types. This is really bad news, because the linear
`ifThenElse` has a different type and different semantics, that
_cannot_ be implemented as a user.

The consequence of this is that merely by activating a compiler extension,
previously legitimate code ceases to be able to compile, which is horrible
since we straight up lose well-typed code. Adding insult to injury, there is
even _no way_ to reconcile the two different types, so an internal fix of this
for linearity would just reverse the issue by causing the same problems for
the regular, less constrained `ifThenElse`.


Thanks for reading! Feel free to drop a comment in the [reddit discussion
thread](https://www.reddit.com/r/haskell/comments/6t1zp1/usability_issues_with_linear_haskell_soh/)!
