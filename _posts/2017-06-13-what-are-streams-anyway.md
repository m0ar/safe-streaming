---
layout: post
title: What are streams anyway?
---
The `streaming` streams are simply an effectful sequence of values, followed by a result. Think list, but with the possiblility to run monadic actions to produce the values. From the generality of monads, this could be reading input
from keyboard, waiting for a server reply, asking some state, reading a mind,
et cetera. 

What is particularily nice is that values are produced _on the fly_, meaning we can receive values as soon as they are ready and send them into some some computational pipeline, like examplified in the package documentation:

{% highlight haskell %}
 >>> S.stdoutLn $ S.map (map toUpper) $ S.take 2 S.stdinLn
 hello<Enter>
 HELLO
 world!<Enter>
 WORLD!
{% endhighlight %}

This code waits for standard input (two times), and as soon as it gets a
value, the string is capitalized and printed without waiting for the rest of
the input.

Show me the types!
------------------

The type of a stream is 
{% highlight haskell %}
Stream f m r
{% endhighlight %}

where 
* `f` is a functor describing the shape of our data; this means we can stream anything we can `fmap`, like lists and trees but also IO actions (and "non-collection" functors like `Maybe` and `Either a`, which might make less practical sense).
* `m` is a monad where the stream elements may perform effects. If it is `State` the stream can query and update some state while evaluating it's elements, if it is `IO` it can interact with the outside world, and so on.
* `r` is the type of value at the end of the stream, which can be of another type than the stream elements. In case the stream is infinite it simply never appears, so in that case, and where you simply don't care, the unit type `()` is a common choice.

In the `Streaming.Prelude` library, which mimics the Haskell prelude's list API, the less general functor

{% highlight haskell %}
data Of a r = !a :> r
{% endhighlight %}

is used for `f`. This means that the stream models a left-strict pair of some `a` (probably a functor, but does not have to be) followed by the "end of stream" type value. The reason for this less general type is that it allows an API closely related to Haskell lists, pipes, conduit, and io-streams. However, for enabling greater flexibility and to easily be able to express nested streams the polymorphic `f` is needed as a base type. Nested streams in particular is very useful for defining things like chunking and concatenation of such streams.




OK, so why do I need this?
-----------------------

The main gain with streaming _in general_ is that you get values on the fly, whereas using the classical `mapM` with friends to get monadic values will only get you results after the whole data structure is gobbled up onto the heap. Constant-space transformations like this is possible with lists (with ordinary composition + compiler optimisation) if we _disallow_ the monadic effects, which was what we wanted to gain (some of it can be done with `lazy-IO`, but this approach has been shown to be inpredictible and error-prone).

We also get nice composability since we can have streams of streams, and there are a plethora of higher-order streaming combinators which allow expressive operations on both flat and nested streams.

They are efficient since the values are returned on the fly, the full data structure never has to be allocated in memory (if not explicitly expressed), meaning better memory behaviour. It also means that there are no intermediate data structures contributing to overhead. This is also done for lists in the compiler, a process known as deforestation, but it is difficult to know beforehand in what way GHC will optimise a certain computaion which makes reasoing about performance difficult.

Compared to lists, streams are simply more powerful since they can perform monadic action to produce its elements, and are also functor general where lists are shaped like... lists.

On the flip side, the types might get a bit more obscure, but on the other hand the programs often get simpler. It might also be harder for GHC to take advantage of the sharing property of lazy evaluation in a streaming context, so that might affect performance in some contexts.

