---
layout: post
title: Usability issues with linear Haskell
---

Working with linearly typed Haskell has been really interesting, and I have
leveraged a [linear monad](https://m0ar.github.io/safe-streaming/2017/07/20/homegrown-linear-monads.html)
for much of my endeavours. This has been neat in combination with
`-XRebindableSyntax` to get do-notation to plug-and-play, but not everything
has been a smooth experience. In this post I will document a few of the
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

Both these examples _should_ work, and linear versions of both `case` and
`let` will indeed be implemented later. For now, one can work around these
hurdles through adding where-functions and doing the `case` logic through
pattern matching there.


## Mixing monad classes
So, this `-XRebindableSyntax` thing made using a redefined monad a real hoot.
Until you need to mix them in the same module... Then you end up with
qualified monadic infix operators in a sea of lambdas, because the
`do`-notation can only be bound to the functions of _one_ of the monad
classes. This could be a common issue, because you possibly do not care about
the linearity of _everything_, but only in parts of your code. When
`do`-notation only works for one of your monads, you have a few choices:


* Figure out which of your monads you need to write the simplest code for,
  then write that without `do`-notation using qualified infix monadic
  operators in a sea of lambdas. This only to realise that is was more complex
  than expected, so you change your mind and have to rewrite _that_ in
  `do`-notation and change the _other_ code to qualified infix riddled
  spaghetti.

* Split up a logically sound module into two separate ones, one for linear
  code and one for unrestricted. Then you get screwed over by recursive
  dependencies, confused by your own code and/or bullied in code review.

* Pack your favourite monad in a record and take it with you! Use it to
  override `do`-notation locally, it's both practical _and_ fun!

{% highlight haskell %}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DatatypeContexts #-}

data LMonad m =>
  LinearMonadOverloader m a b = MO
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

Or we could try to figure out...


### A proper solution idea
It should be possible to solve this by creating a sort of "monad umbrella",
allowing different multiplicity monads to share a common interface, which can
then be used for functions that want to support both of them. I have been
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

type family Arrow p a b where
  Arrow One   a b = a ⊸ b
  Arrow Omega a b = a -> b

class MonadWithWeight (p :: Multiplicity) m where
  return :: Arrow p a (m a)
  (>>=)  :: Arrow p (m a) (
              Arrow p (Arrow a (m b))
                m b)

instance Monad m => MonadWithWeight 'Omega where
  return = M.return
  (>>=) = (M.>>=)

instance LMonad m => MonadWithWeight 'One where
  return = L.return
  (>>=) = (L.>>=)
{% endhighlight%}

If we did something like this, we could bind the `do`-notation to the
functions provided by `MonadWithWeight` and automagically get the correct
implementation depending on which monad we are currently typed with!


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
The arguaby nastiest issue is related to the `if-then-else` construct, and
is much worse way than `case` and `let`. The core syntax _is_ implemented
linearly, but clashes with `-XRebindableSyntax`; as soon as this extension is
activated, the internal `if-then-else` goes out of scope and you have to
provide a function `ifThenElse :: a -> b -> c -> d` yourself.

This is clearly fully unrestricted and _cannot_ be used to implement a linear
`if-then-else`, what ever you do you cannot satisfy the linearity checker. The
consequence of this is that merely by activating a compiler extension,
previously legitimate code ceases to be able to compile, which is *very* bad
since we straight up lose well-typed code by activating an extension.
Unfortunately there is _no way_ to fix this with library functions, so it
would need to be patched in GHC.


Thanks for reading, feel free to drop a comment in the [reddit discussion
thread](placeholderlink)!
