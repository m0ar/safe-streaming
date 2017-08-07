---
layout: post
title: Take & Zip: An issue with linearity
---

When adapting the `streaming` library for better type safety by leveraging
linearity, one particular set of functions has been, and still is, a major
issue: `take`, `zip` and everything based on them.

Basically, the whole point of linear streams is that we _know_ that the
monadic actions downstream are guaranteed to be performed (see earlier posts
on [linear types & streams](https://m0ar.github.io/safe-streaming/2017/06/19/linear-types-101.html)
and [linear monads](https://m0ar.github.io/safe-streaming/2017/07/20/homegrown-linear-monads.html)
if you are curious on why this is). Skipping the resulting stream _elements_
is usually not an issue, and can be done neatly with unrestricted constructors
in the stream functor
([like so](https://github.com/m0ar/safe-streaming/blob/master/src/Data/Functor/LOf.hs#L21)),
but we really need the dowstream _actions_ done! This unfortunately
makes it very tricky for the cases when we genuinely do not _want_ the rest of
the values of the stream, as in the semantics of the core functions `take` and
`zip`. Since this is a very common pattern in the programming model of
streams, this indeed seems to be a big issue.



### The problem with `take`
The type of `take` in the original `Streaming.Prelude` is

{% highlight haskell%}
take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
{% endhighlight %}

and the semantics are simple: we yield `k` elements from the given stream
through performing actions in some monad, before we abruptly stop and discard
both the rest of the stream (with its corresponding sequence of actions) and
the end-of-stream value of type `r`.

We sometimes want to compute on a stream until we find an element fulfilling a
predicate, only consider a subsequence of values then quit, or work with
infitite streams (which often get cumbersome in absence of `take`-like
functions). A simple example could be something along the lines of

{% highlight haskell %}
import Streaming.Prelude (takeWhile, read, stdinLn)

userSmallNumbers :: Stream (Of Int) IO ()
userSmallNumbers = takeWhile ((<) 100) . read . stdinLn

-- stdinLn :: MonadIO m => Stream (Of String) m ()
-- Endless supply of standard input

-- read :: (Monad m, Read a) => Stream (Of String) m r -> Stream (Of a) m r
-- Tries to parse a's from the stream of String, skipping failed elements

-- takeWhile :: (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
-- Continue yielding while the elements fulfill the predicate
{% endhighlight%}

You can probably imagine how useful this pattern is, so not being able to
`take` linear streams is indeed very troubling.



### The problem with `zip`
`zip` is also interesting since the original implementation does two things
that are illegal in the linear setting:

1. Throws away the tail of the longer stream (matches the semantics of
   `Prelude.zip`)
2. Discards the end-of-stream value of the longer stream

The first issue is shared with `take`, while the other could with equal
length streams be solved by ending with a tuple of both stream end values.

You have probably used `zip` in different settings, but I suspect you usually
don't care about the tail of the longer data structure, or even leverage these
semantics with infinite data structures. A lot of the power and applicability
of `zip` comes from this, so it is indeed an issue that we cannot implement it
with linear streams.



## How can we approach these issues?
To really solve the problem with `take` we would need to create some
abstraction for _abortable streams_, so we can know that the stream is fine
with being cut early since this is not true in the general case.


### Destruction of linear stream values
If we would allow the stream itself to be aborted, we no longer treat its
yielded elements linearly either, hence a destructible stream implies
destructible values. One way to solve this is to only allow streaming
unrestricted values, but that seems a bit harsh. Another way could be to have
some type class for _destructability_ of linear values, where each type can
decide how you consume its elements:

{% highlight haskell %}
class Destructible a where
  destroy :: a ⊸ ()
{% endhighlight %}

For nullary constructors this can be implemented just by
pattern matching, since this is enough to consume those values:

{% highlight haskell %}
instance Destructible Bool where
  destroy :: Bool ⊸ ()
  destroy True = ()
  destroy False = ()
{% endhighlight%}


For more interesting data types, we can implement `destroy` if the type of
fields implement `Destructible` too:

{% highlight haskell %}
instance Destructible a => Destructible [a] where
  destroy [] = ()
  destroy (x:xs) = destroyRest $ destroy x
  where destroyRest :: () ⊸ ()
        destroyRest () = destroy xs
{% endhighlight%}


It is also possible to extend this to primitive types (with a _bit_ of
acrobatics):

{% highlight haskell %}
{-# LANGUAGE GADTs #-}
-- Enables #-syntax needed for unboxed types
{-# LANGUAGE MagicHash #-}

import GHC.Prim (Int#, (+#), (*#))

-- Can't write instances for unboxed types,
-- so let's hide it inside here
data Int where Int# :: Int# -> Int

instance Destructible Int where
  destroy (Int# _) = ()


-- This isn't really necessary to convey my point, but if you
-- want to try this GHC needs to understand that your Int is a
-- Num, or it'll haunt you in confusion over your literals:
import GHC.Classes (compareInt#)
instance Num Int where
  (Int# x) + (Int# y) = Int# (x +# y)
  (Int# x) * (Int# y) = Int# (x *# y)
  negate (Int# x) = Int# (negateInt# x)
  abs i@(Int# x) = if x `compareInt#` 0# == LT then negate i else i
  signum (Int# x) = if x `compareInt#` 0# == LT then -1 else 1
  fromInteger = Int# . (fromInteger :: Integer -> GHC.Types.Int)
{% endhighlight%}


For more exotic types you might have to resort to using `unsafeCoerce` in
different ways, which may bring its own problems.



### Destruction of a `Stream`
We have seen how we can consume linear variables yielded by a stream pretty
smoothly, which is neat, but the main problem still stands: the effects
downstream. When really aborting the stream we are discarding all upcoming
effects, and this is in conflict with the guarantees that we have achieved
with the linearity!

This is just a sketch of an idea, but to model this "optional advancement" we
could use a special monad embracing just that, by providing a continuation
semantic along the lines of `Bool -> Maybe (m a)`. Give it `False` and it
returns `Nothing`, ending the computation. A `True` and it returns `Just (m
a)`, from where we can start over. If our stream is safe to exit at any point,
we can make it an instance of this monad and make that explicit. This would
again require that the stream functor is an instance of `Destructible`,
because it implies not using the elements the stream yields.


{% highlight haskell %}
class Exitable m where
  step :: forall a. Bool -> m a ⊸ Maybe (m a)
{% endhighlight%}


### Any hope for `zip`?
`zip` remains tricky because it's locked until we can `take` properly, since
it suffers from the same problems. An different approach all together could
include encoding the stream length in the types; this would be nice in the
sense that we could define `zip` only on streams of equal length. On the other
hand, this would probably not be very practical nor fit the programming model
very well since we often _leverage_ that streams are of different lengths, and
often simply don't know beforehand.


That's it for today! Feel free to drop comments or questions at
[reddit](placeholder), I'd love to hear other ideas on these issues.
