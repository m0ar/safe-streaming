---
layout: post
title: Take & Zip — An issue with linear streams
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
([like so](https://github.com/m0ar/safe-streaming/blob/3b488017ab97537f5f78c5f50e64329fef413e4c/src/Data/Functor/LOf.hs#L16-L24),
but we really need the dowstream _actions_ done! This unfortunately makes it
tricky for the cases when we genuinely do not _want_ the rest of the values of
the stream, as in the semantics of the core functions `take` and `zip`. Since
this is a common pattern in the programming model of streams, this indeed
seems to be a big issue.


## The problem with `take`
We sometimes want to compute on a stream until we find an element fulfilling a
predicate, only consider a subsequence of values then quit, or work with
infinite streams (which often get cumbersome in absence of `take`-like
functions). The type of `take` in the original `Streaming.Prelude` is

{% highlight haskell%}
take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
{% endhighlight %}

and the semantics are simple: we yield `k` elements from the given stream
through performing actions in some monad, before we abruptly stop and discard
both the rest of the stream (with its corresponding sequence of actions) and
the end-of-stream value of type `r`. Unfortunately, this obviously cannot be
linear, and this is for a good reason: what if there is a `closeFile` action
further down the stream?  If we cut the stream early, that action will not be
performed and we have made ourselves a resource leak!

If the stream is completely unrestricted, this isn't a problem. Taking what
you need from a pure stream like this should be okay, since the yielded
elements are unrestricted, there are no side effects and no end-of-stream
value:

{% highlight haskell %}
import Streaming.Prelude (each, stdoutLn, show, take)

printInts :: Int -> IO ()
printInts k =
  let s = each [1..] :: Stream (LOf Integer) Identity ()
  in stdoutLn $ show $ take k s
{% endhighlight%}


So should we constrain ourselves to unrestricted monads then? No, since that
would severely restrict the possible streams and would make simple programs
like this impossible:

{% highlight haskell %}
import Streaming.Prelude (takeWhile, read, stdinLn)

userSmallNumbers :: Stream (Of Int) IO ()
userSmallNumbers = takeWhile ((<) 100) . read . stdinLn

-- stdinLn :: Stream (Of String) IO ()
-- Endless supply of standard input

-- read :: (Monad m, Read a) => Stream (Of String) m r -> Stream (Of a) m r
-- Tries to parse a's from the stream of String, skipping failed elements

-- takeWhile :: (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
-- Continue yielding while the elements fulfill the predicate
{% endhighlight%}


You can probably imagine how useful this pattern is, so not being able to
`take` linear streams is indeed troubling. But couldn't we just use its
linear cousin, `splitsAt`?

{% highlight haskell %}
splitsAt :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m (Stream f m r)
{% endhighlight%}

It transforms an infinite stream into a finite one, and lets you snag what
you want from the head. Isn't that exactly what we need? Unfortunately no; as
you can see in the type the linear end-of-stream value is another stream, the
remainder of after splitting, and we need to consume this too!

To really solve the problem with `take` we would need to create some
abstraction for destructible streams. This way we could know that the stream
is fine with being cut early, which is not true in the general case. As a
first step, let us take a look at how we can destroy linear variables at all,
since it sounds pretty weird.


### Destruction of linear values
Our problems would be solved if we could have a `destroy :: Stream f m r ⊸ ()`,
but let's start by figuring out how we can destroy linear values of simpler
types. Let's create a type class for _destructability_ of linear values, where
each type can decide how you can consume its inhabitants (which really makes
them _affine_):

{% highlight haskell %}
class Destructible a where
  destroy :: a ⊸ ()
{% endhighlight %}


For nullary constructors this can be implemented just by pattern matching,
since this is enough to consume those values:

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


It is also possible to extend this to primitive types with a bit of
acrobatics. This is mostly a fun experiement, in the future primitive types
like `Int` will come with `destroy` primitives.

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

-- If you want to play with this in GHCi you need
-- to use primitive literal syntax for your ints
{% endhighlight%}


For more exotic types you might have to resort to using `unsafeCoerce` in
different ways, which may bring its own problems.


### Destruction of a `Stream`
Now we have seen how we can consume linear variables pretty smoothly which is
neat and all, but the main problem still stands: the effects downstream could
be doing critical stuff so we can't just skip them, and what _consumption_ of
a monadic value even _means_ depends on the monad! When really aborting the
stream we are discarding all of the actions, and this is in conflict with the
guarantees that we have achieved with the linearity.

Unfortunately it is still not clear precicely how we need to constrain our
`m` to make it possible to implement `destroy :: Stream f m r ⊸ ()`. A start
would be to create an abstraction of "optional continuations" of monadic
computations, where we can keep the linearity of the monadic values but allow
the computation to exit somewhere. So if we create some `Affine a`, a monad
along these lines could be one way to enable `take` on streams:


{% highlight haskell %}
class AffineMonad m where
  (>>=) :: Affine (m a) ⊸ Affine (Affine a ⊸ m b) ⊸ m b
{% endhighlight %}

This is just a sketch, but something like it could allow keeping the monadic
values linear by only allowing linear transformations, but you can quit
anywhere by `destroy`-ing the `a` and not using the continuation. Several
questions remain though:

* Does this have reasonable and practical instances?
* How it would mesh with corresponding functors and applicatives?


It might be possible to make `ResourceT` an instance of `AffineMonad`, which
would allow safely aborting a monad with shared resources, relying on
`ResourceT` to cleanup our leftovers.



## The problem with `zip`
`zip` is also interesting since the original implementation does two things
that are illegal in the linear setting:

1. Throws away the tail of the longer stream (matches the semantics of
   `Prelude.zip`)
2. Discards the end-of-stream value of the longer stream

The first issue is shared with `take`, while the other could with equal length
streams be solved either by ending with a tuple of both stream end values, or
combining them with some function `f :: r ⊸ r ⊸ r`.

You have probably used `zip` in different settings, but I suspect you
sometimes don't care about the tail of the longer data structure, or even
leverage these semantics with infinite data structures. A lot of the power and
applicability of `zip` comes from this, so it is indeed an issue that we
cannot implement it with linear streams.

`zip` will remain tricky because it's locked until we can `take` properly,
since it suffers from the same problems. A different approach all together
could include encoding the stream length in the types; this would be nice in
the sense that we could define `zip` only on streams of equal length. On the
other hand, it would probably not be very practical, nor fit the programming
model particularly well since we often _leverage_ that streams are of
different lengths, and often simply don't know beforehand.


That's it for today! Feel free to drop comments or questions at
[reddit](https://www.reddit.com/r/haskell/comments/6sdwak/take_zip_an_issue_with_linear_streams/),
I'd love to hear other ideas on these issues.
