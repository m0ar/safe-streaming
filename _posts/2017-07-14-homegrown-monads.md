---
layout: post
title: Homegrown linear monads with RebindableSyntax
---

When tackling the before-mentioned problem with implicitly repeated effects in
streams, the need for a _linear_ monad class arose. This would allow relying on the
type system to ensure that a monadic value can only be used once by forcing
the bind (`>>=`) to consume its first argument, effectively
making the earlier erroneous examples impossible.

This was interesting because monads and their accompanied `do`-notation feels
very tightly coupled with Haskell as a language, but the GHC extension
`-XRebindableSyntax` allows quite some tinkering!

### Meet LMonad
First I defined the linear monad, because it was the monadic semantics that I
needed to stricten:

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

The `>>=` is pretty straight forward: given a monadic value, we apply a
_linear_ tranformation _once_, and get a new monadic value.

`>>` looks a bit more different than we are used to, and in particular very
restrictive! This is logical though, since the semantics of `>>` is to perform
a computation and throw away the result, something not very linear at all.
Therefore it only makes sense to blind something that does not produce
results, namely values of type `m ()`.

`return` is `pure` from a linear `Applicative` because I wanted to keep the
heritage line of subclasses: `Functor > Applicative > Monad` (this is also why
I have included `ap`). Let's follow the types and see where they take us for
the superclasses!

### LApplicative
Now we want to define of a linear Applicative, which is already somewhat
constrained from the types of `LMonad`. We know from the laws of the universe
(ok, Haskellised category theory) that `Applicative` and `Monad` should relate
through these two laws:

{% highlight haskell %}
pure = return
(<*> = ap)
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
{% endshighlight %}

Here we get the types for `pure` and `ap` for free from the above laws. 

For `*>` and `<*` we face the same restrictions as for `LMonad`: we cannot
throw away values! Hence, the only reasonable approach is to only allow
replacing values of type `()`. Now let's take a look where this leads us for
the `Functor`.


### LFunctor
It would sure be nice to have `fmap :: (a ⊸ b) -> f a ⊸ f b` (note the regular
arrow), letting us transform all values in the functor with a single linear
function. But unfortunately that would not allows us to use `fmap` on linear
monads, because it will have an unrestricted (nonlinear) type! So we are
confined to applying the function on _a single value_:

{% highlight haskell %}
class LFunctor f where
  fmap :: (a ⊸ b) ⊸ f a ⊸ f b

  (<$) :: a ⊸ f () ⊸ f a
  (<$) = fmap . \a () -> a
{% endhighlight %}

As it turns out, this does not seem to be the end of the world, at least not
the world of `streaming`. The functor that is used in the streaming
prelude looks like this:

{% highlight haskell %}
data Of a b = !a :> b

instance Functor (Of a) where
  fmap f (a :> x) = a :> f x
  a <$ (b :> x)   = b :> a
{% endhighlight %}

where `b` is the end-of-stream value. ...hey, that isn't half bad: `fmap` _is_
only applied to single values! To solve the problems with streaming we want
to constrain the _effects_ of the stream, usually we don't care about the
linearity of the _values_. So, with this definition of a semi-linear `Of` we
can even use the same `Functor` instance and be on our way:

{% highlight haskell %}
data LOf a b where (:>) :: a -> b ⊸ LOf a b
{% endhighlight %}


### I was promised extension hacking!
Ah yes, so how can we use these without having to resort to less-than-stellar
code with qualified infix operators `L.>>=` mixed with crazy lambdas
everywhere? Say hi to `-XRebindableSynax`! This little gem allows you to
rebind much of the syntax to your own definitions (pretty well explained in
further detail [here](https://ocharles.org.uk/blog/guest-posts/2014-12-06-rebindable-syntax.html)).

So everything we need to do to actually use our new and shiny monad is to hide
`>>, >>=, return` with friends from prelude, activate the extension, import
our monad class and do-notation works magically out of the box:

{% highlight haskell %}
{-# LANGUAGE RebindableSyntax #-}

import Control.Monad.LMonad
import Prelude hiding ((>>), (>>=), return)

EXAMPLE HERE... !?

{% endhighlight %}
