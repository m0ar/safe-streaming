---
layout: post
title: Homegrown linear monads with RebindableSyntax
---

When tackling the before-mentioned problem with repeated effects in
streams, the need for a _linear_ monad class arose. This would allow relying
on the type system to ensure that a monadic value can only be used once by
forcing the bind (`>>=`) to consume its first argument (disallowing multiple
uses of earlier stream references), effectively making the earlier erroneous
examples impossible.

This was interesting because monads and their accompanied `do`-notation feels
very tightly coupled with Haskell as a language, but the GHC extension
`-XRebindableSyntax` allows quite some tinkering!

If you want to code along and tinker with the lollipops yourself, I helped
the guys at Tweag I/O write a small guide to get started which can be found
[here](https://github.com/tweag/ghc/blob/linear-types/README.md).


### Meet LMonad!
First I defined the linear monad, because it is the monadic semantics that I
need to stricten:

{% highlight haskell %}
class LApplicative m => LMonad m where
  (>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b

  (>>) :: m () ⊸ m a ⊸ m a
  m >> k = m >>= \() -> k
  {-# INLINE (>>) #-}

  return :: a ⊸ m a
  return = pure

ap :: LMonad m => m (a ⊸ b) ⊸ m a ⊸ m b
ap m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (x1 x2)
{% endhighlight %}

The `>>=` is pretty straight forward: given a monadic value, we unwrap it,
apply it to a _linear_ morphism _once_, and get a new monadic value. One of my
first hunches here was to use `(>>=) :: m a ⊸ (a -> m b) ⊸ m b` which would
make the superclasses a lot less constrained, but unfortunately we need to
take in account that the `a` here may also be some monadic value
(e.g. `a ~ Maybe Int`, and *particularly* `a ~ Stream f m r`). Allowing this
unrestricted continuation `a -> m b` would allow freely duplicating those
monadic actions.

`>>` looks a bit more different than we are used to, and in particular very
restrictive! This is logical though, since the semantics of `>>` is to perform
a computation and throw away the result, something not very linear at all.
Therefore it only makes sense to blind something that does not produce
results, namely values of type `m ()`.

`return` is `pure` from a linear `Applicative` because I want to keep the
heritage line of subclasses: `Functor > Applicative > Monad` (this is why I
have included `ap`, which will be clear in a moment). Let's follow the types
and see where this takes us for the superclasses!


### LApplicative
Now we want to define of a linear Applicative, which is already somewhat
constrained from the types of `LMonad`. We know from the monad laws that
`Applicative` and `Monad` should relate through these two equations:

{% highlight haskell %}
pure  = return
(<*>) = ap
{% endhighlight %}

...which already hints us with the correct types for `LApplicative`:

{% highlight haskell %}
class LFunctor f => LApplicative f where
  pure :: a ⊸ f a

  (<*>) :: f (a ⊸ b) ⊸ f a ⊸ f b

  (*>) :: f () ⊸ f b ⊸ f b
  a1 *> a2 = (id <$ a1) <*> a2

  (<*) :: f a ⊸ f () ⊸ f a
  (<*) = liftA2 $ \a () -> a

  liftA2 :: LApplicative f => (a ⊸ b ⊸ c) ⊸ f a ⊸ f b ⊸ f c
  liftA2 f a b = f <$> a <*> b
{% endhighlight %}

Here we get the types for `pure` and `ap` for free from the above laws,
since they need to be the same as for their `LMonad` equivalents.

For `*>` and `<*` we face the same restrictions as for `LMonad`: we cannot
throw away values! If we do not respect this, we cannot use `LApplicative`
operations on any `LMonad`, because they will break the linearity constraints.
Hence, the only reasonable approach is to only allow replacing values of type
`()`.

> Worth noting if you are hacking along is that `$` isn't the `prelude` one,
> but a [linear counterpart](https://github.com/m0ar/safe-streaming/blob/master/src/Data/Linear.hs).
> This is a returning problem at the moment; since the linearity checker does
> not infer linearity if not explicitly told so, instead it assumes functions
> to be unrestricted which leads to
> `hey, this variable should be treated linearly`-errors. This is explained
> further in the readme linked in the introduction.

Now let's take a look where this leads us for the `LFunctor`!


### LFunctor
It would sure be nice to have `fmap :: (a ⊸ b) -> f a ⊸ f b` (note the regular
arrow), letting us transform all values in the functor with a single linear
function. But unfortunately that would not allows us to use `fmap` on linear
monads, because it will have an unrestricted (nonlinear) type! The linearity
checker cannot allow this, because it is not sure the values are treated
linearly. So we are confined to:

{% highlight haskell %}
class LFunctor f where
  fmap :: (a ⊸ b) ⊸ f a ⊸ f b

  (<$) :: a ⊸ f () ⊸ f a
  (<$) = fmap . \a () -> a
{% endhighlight %}

Here we almost have the good old `fmap`, but with a linear transformation
which can only be applied _once_. For `(<$)` the same logic applies as for
`LApplicative`: we can't throw away values, so the only values we can replace
are values of type unit.

It sounds crippling, but as it turns out this does not seem to be the end of
the world. For example, the functor that is used for representing the shape of
streamed elements in the `streaming` prelude looks like this:

{% highlight haskell %}
data Of a b = !a :> b

instance Functor (Of a) where
  fmap f (a :> x) = a :> f x
  a <$ (b :> x)   = b :> a
{% endhighlight %}

where `b` is the end-of-stream value. ...hey, that isn't half bad: `fmap` _is_
only applied to a single value, but the functor can hold any type of values in
its unrestricted `a`! To solve the problems with streaming we want to
constrain the _effects_ of the stream, usually we don't care about the
linearity of the _values_. So, since we in this context don't really care
about linearity in `a`, we might as well just make it unrestricted (note the
regular arrow in the constructor):

{% highlight haskell %}
data LOf a b where (:>) :: a -> b ⊸ LOf a b

instance LFunctor (LOf a) where
  fmap f (a :> x) = a :> f x
{% endhighlight %}

With this definition of a partly-linear `Of` we can even use the same
`Functor` instance (with the above default implementation of `(<$)`) and be
on our way!


### I was promised extension hacking!
Ah yes, so how can we use these without having to resort to
less-than-stellar-readability code with qualified infix operators `L.>>=`
mixed with crazy lambdas everywhere? Say hi to `-XRebindableSynax`! This
little gem of an extension allows you to rebind much of the Haskell syntax
to your own definitions (pretty well explained in further detail
[here](https://ocharles.org.uk/blog/guest-posts/2014-12-06-rebindable-syntax.html),
a neat advent calendar with a new GHC extension every day).

So everything we need to do to actually use our new and shiny monad is to hide
`>>`, `>>=`, `return` from prelude, activate the extension, import
our monad class and do-notation works magically out of the box:

{% highlight haskell %}
{-# LANGUAGE RebindableSyntax #-}

import Control.Monad.LMonad
import Prelude hiding ((>>), (>>=), return)

instance (LFunctor f, LMonad m) => LApplicative (Stream f m) where
  pure = return
  {-# INLINE pure #-}
  streamf <*> streamx = do
    f <- streamf
    x <- streamx
    return (f x)
  {-# INLINE (<*>) #-}

{% endhighlight %}

The example is taken from my [linear streaming fork](https://github.com/m0ar/safe-streaming/blob/master/src/Streaming/Internal.hs).

This extension is very clever and enables friction-free use of whatever
definition you want to use for `>>=`, `>>` and `return` (other syntactic
stuff too!).
