{-# LANGUAGE LambdaCase, RankNTypes, BangPatterns #-}
module Streaming.Prelude (
    -- * Types
    Stream 
    , Of (..)
    
    -- * Introducing streams of elements
    -- $producers
    ,  stdinLn
    , readLn
    , fromHandle
    , repeatM
    , replicateM
    , each
    , yield

    -- * Consuming streams of elements
    -- $consumers
    , stdoutLn
    , stdoutLn'
    , mapM_
    , print
    , toHandle
    , drain

    -- * Stream transformers
    -- $pipes
    , map
    , mapM
    , sequence
    , mapFoldable
    , filter
    , filterM
    , for
    , take
    , takeWhile
--    , takeWhile'
    , drop
    , dropWhile
    , concat
    -- , elemIndices
    -- , findIndices
    , scan
    , scanM
    , chain
    , read
    , show
    , seq

    -- * Splitting streams of elements
    , splitAt
    , break
    , span
    
    -- * Folds
    -- $folds
    -- * Complete folds
    , fold
    , fold'
    , foldM
    , foldM'
    , sum
    , sum'
    , product
    , product'
    , toList
    , toListM
    , toListM'
    
    -- * Short circuiting folds
    -- , all
    -- , any
    -- , and
    -- , or
    -- , elem
    -- , notElem
    -- , find
    -- , findIndex
    -- , head
    -- , index
    -- , last
    -- , length
    -- , maximum
    -- , minimum
    -- , null

    -- * Zips
    , zip
    , zipWith

  ) where
import Streaming.Internal

import Control.Monad hiding (filterM, mapM, mapM_, foldM, replicateM, sequence)
import Data.Functor.Identity
import Control.Monad.Trans
import qualified Prelude as Prelude                      
import qualified Data.Foldable as Foldable
import Text.Read (readMaybe)
import Prelude hiding (map, mapM, mapM_, filter, drop, dropWhile, take, sum, product
                      , iterate, repeat, replicate, splitAt
                      , takeWhile, enumFrom, enumFromTo
                      , print, zipWith, zip, seq, show, read
                      , readLn, sequence, concat, span, break)

import qualified GHC.IO.Exception as G
import qualified System.IO as IO
import Foreign.C.Error (Errno(Errno), ePIPE)
import Control.Exception (throwIO, try)

-- ---------------
-- ---------------
-- Prelude
-- ---------------
-- ---------------

break :: Monad m => (a -> Bool) -> Stream (Of a) m r 
      -> Stream (Of a) m (Stream (Of a) m r)
break pred = loop where
  loop str = case str of 
    Return r         -> Return (Return r)
    Delay m          -> Delay $ liftM loop m
    Step (a :> rest) -> if (pred a) 
      then Return (Step (a :> rest))
      else Step (a :> loop rest)
{-# INLINEABLE break #-}

{-| Apply an action to all values flowing downstream

> let debug str = chain print str
-}
chain :: Monad m => (a -> m ()) -> Stream (Of a) m r -> Stream (Of a) m r
chain f str = for str $ \a -> do
    lift (f a)
    yield a
{-# INLINE chain #-}



concat :: (Monad m, Foldable f) => Stream (Of (f a)) m r -> Stream (Of a) m r
concat str = for str each
{-# INLINE concat #-}

-- ---------------
-- drain
-- ---------------

-- | Reduce a stream, performing its actions but ignoring its elements.
drain :: Monad m => Stream (Of a) m r -> m r
drain = loop where
  loop stream = case stream of 
    Return r         -> return r
    Delay m          -> m >>= loop 
    Step (_ :> rest) -> loop rest

-- ---------------
-- drop
-- ---------------

-- | Ignore the first n elements of a stream, but carry out the actions
drop :: (Monad m) => Int -> Stream (Of a) m r -> Stream (Of a) m r
drop = loop where
  loop n stream 
    | n <= 0    = stream
    | otherwise = case stream of
      Return r       -> Return r
      Delay ma       -> Delay (liftM (loop n) ma)
      Step (a :> as) -> loop (n-1) as
{-# INLINEABLE drop #-}

-- ---------------
-- dropWhile
-- ---------------

-- | Ignore elements of a stream until a test succeeds.
dropWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
dropWhile pred = loop where 
  loop stream = case stream of
    Return r       -> Return r
    Delay ma       -> Delay (liftM loop ma)
    Step (a :> as) -> if pred a 
      then loop as
      else Step (a :> as)
{-# INLINEABLE dropWhile #-}

-- ---------------
-- each 
-- ---------------

-- | Stream the elements of a foldable container.
each :: (Monad m, Foldable.Foldable f) => f a -> Stream (Of a) m ()
each = Foldable.foldr (\a p -> Step (a :> p)) (Return ())
{-# INLINE each #-}

-- -----
-- enumFrom
-- ------

enumFrom :: (Monad m, Num n) => n -> Stream (Of n) m ()
enumFrom = loop where
  loop !n = Step (n :> loop (n+1))
{-# INLINEABLE enumFrom #-}

enumFromTo :: (Monad m, Num n, Ord n) => n -> n -> Stream (Of n) m ()
enumFromTo = loop where
  loop !n m = if n <= m 
    then Step (n :> loop (n+1) m)
    else Return ()
{-# INLINEABLE enumFromTo #-}

enumFromStepN :: (Monad m, Num a) => a -> a -> Int -> Stream (Of a) m ()
enumFromStepN start step = loop start where
    loop !s m = case m of 
      0 -> Return ()
      _ -> Step (s :> loop (s+step) (m-1))
{-# INLINEABLE enumFromStepN #-}

-- ---------------
-- filter 
-- ---------------

-- | Skip elements of a stream that fail a predicate
filter  :: (Monad m) => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filter pred = loop where
  loop !str = case str of
    Return r       -> Return r
    Delay m        -> Delay (liftM loop m)
    Step (a :> as) -> if pred a 
                         then Step (a :> loop as)
                         else loop as
{-# INLINEABLE filter #-}

-- ---------------
-- filterM
-- ---------------

-- | Skip elements of a stream that fail a monadic test
filterM  :: (Monad m) => (a -> m Bool) -> Stream (Of a) m r -> Stream (Of a) m r
filterM pred = loop where
  loop str = case str of
    Return r       -> Return r
    Delay m        -> Delay $ liftM loop m
    Step (a :> as) -> Delay $ do 
      bool <- pred a
      if bool 
        then return $ Step (a :> loop as)
        else return $ loop as
{-# INLINEABLE filterM #-}
-- ---------------
-- fold
-- ---------------

{- $folds
    Use these to fold the elements of a 'Stream'.  The general folds 'fold', fold\'',
    'foldM' and 'foldM\'' are arranged for use with 'Control.Foldl' All functions marked
    with a final '\'' (e.g. 'fold\'', 'sum\') carry the stream's return value -- or, in
    the case of 'maps\'' are tailored to take such an operation as argument.

>  maps' sum' :: (Monad m, Num n) => Stream (Stream (Of n)) m r -> Stream (Of n) m r
>  maps' (fold' mappend mempty id) :: :: (Monad m, Num n) => Stream (Stream (Of n)) m r -> Stream (Of n) m r
-}

{-| Strict fold of a 'Stream' of elements

> Control.Foldl.purely fold :: Monad m => Fold a b -> Stream (Of a) m () -> m b
-}
fold :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m () -> m b
fold step begin done stream0 = loop stream0 begin
  where
    loop stream !x = case stream of 
      Return r         -> return (done x)
      Delay m          -> m >>= \s -> loop s x
      Step (a :> rest) -> loop rest (step x a)
{-# INLINABLE fold #-}

{-| Strict fold of a 'Stream' of elements that preserves the return value

> Control.Foldl.purely fold' :: Monad m => Fold a b -> Stream (Of a) m r -> m (b, r)
-}

fold' :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> m (b, r)
fold' step begin done s0 = loop s0 begin
  where
    loop stream !x = case stream of 
      Return r         -> return (done x, r)
      Delay m          -> m >>= \s -> loop s x
      Step (a :> rest) -> loop rest (step x a)
{-# INLINABLE fold' #-}

{-| Strict, monadic fold of the elements of a 'Stream (Of a)'

> Control.Foldl.impurely foldM :: Monad m => FoldM a b -> Stream (Of a) m () -> m b
-}
foldM
    :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream (Of a) m () -> m b
foldM step begin done s0 = do
    x0 <- begin
    loop s0 x0
  where
    loop stream !x = case stream of 
      Return r         -> done x 
      Delay m          -> m >>= \s -> loop s x
      Step (a :> rest) -> do
        x' <- step x a
        loop rest x'
{-# INLINABLE foldM #-}

{-| Strict, monadic fold of the elements of a 'Stream (Of a)'

> Control.Foldl.impurely foldM' :: Monad m => FoldM a b -> Stream (Of a) m r -> m (b, r)
-}
foldM'
    :: Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> Stream (Of a) m r -> m (b, r)
foldM' step begin done str = do
    x0 <- begin
    loop str x0
  where
    loop stream !x = case stream of 
      Return r         -> done x >>= \b -> return (b, r)
      Delay m          -> m >>= \s -> loop s x
      Step (a :> rest) -> do
        x' <- step x a
        loop rest x'
{-# INLINABLE foldM' #-}

-- ---------------
-- for
-- ---------------

-- | @for@ replaces each element of a stream with an associated stream. Note that the
-- associated stream may layer any functor. 
for :: (Monad m, Functor f) => Stream (Of a) m r -> (a -> Stream f m x) -> Stream f m r
for str0 act = loop str0 where
  loop str = case str of
    Return r         -> Return r 
    Delay m          -> Delay $ liftM loop m
    Step (a :> rest) -> do
      act a
      loop rest
{-# INLINEABLE for #-}

-- ---------------
-- iterate
-- ---------------

-- | Iterate a pure function from a seed value, streaming the results forever
iterate :: (a -> a) -> a -> Stream (Of a) m r
iterate f = loop where
  loop a' = Step (a' :> loop (f a'))
{-# INLINEABLE iterate #-}

-- | Iterate a monadic function from a seed value, streaming the results forever
iterateM :: Monad m => (a -> m a) -> m a -> Stream (Of a) m r
iterateM f = loop where
  loop ma  = Delay $ do 
    a <- ma
    return (Step (a :> loop (f a)))
{-# INLINEABLE iterateM #-}

-- ---------------
-- map
-- ---------------

-- | Standard map on the elements of a stream.
map :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
map f = loop where
  loop stream = case stream of
    Return r -> Return r
    Delay m -> Delay (liftM loop m)
    Step (a :> as) -> Step (f a :> loop as)
{-# INLINEABLE map #-}

-- ---------------
-- mapFoldable
-- ---------------

{-| For each element of a stream, stream a foldable container of elements instead

>>> D.print $ D.mapFoldable show $ D.yield 12
'1'
'2'

-}
mapFoldable :: (Monad m, Foldable t) => (a -> t b) -> Stream (Of a) m r -> Stream (Of b) m r
mapFoldable f str = for str (\a -> each (f a)) -- as in pipes

-- | Replace each element of a stream with the result of a monadic action
mapM :: Monad m => (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapM f = loop where
  loop str = case str of 
    Return r       -> Return r 
    Delay m        -> Delay $ liftM loop m
    Step (a :> as) -> Delay $ do 
      a' <- f a 
      return $ Step (a' :> loop as) 
{-# INLINEABLE mapM #-}


{-| Reduce a stream to its return value with a monadic action.

>>>  mapM_ Prelude.print $ each [1..3] >> return True
1
2
3
True

-}
mapM_ :: Monad m => (a -> m b) -> Stream (Of a) m r -> m r
mapM_ f = loop where
  loop str = case str of 
    Return r       -> return r 
    Delay m        -> m >>= loop
    Step (a :> as) -> do 
      f a 
      loop as 
{-# INLINEABLE mapM_ #-}

-- | Fold a 'Stream' of numbers into their product
product :: (Monad m, Num a) => Stream (Of a) m () -> m a
product = fold (*) 1 id
{-# INLINE product #-}

{-| Fold a 'Stream' of numbers into their product with the return value

>  mapsFold product' :: Stream (Stream (Of Int)) m r -> Stream (Of Int) m r
-}
product' :: (Monad m, Num a) => Stream (Of a) m r -> m (a,r)
product' = fold' (*) 1 id
{-# INLINAE product' #-}

-- ---------------
-- read
-- ---------------

-- | Make a stream of strings into a stream of parsed values, skipping bad cases
read :: (Monad m, Read a) => Stream (Of String) m r -> Stream (Of a) m r
read stream = for stream $ \str -> case readMaybe str of 
  Nothing -> return ()
  Just r  -> yield r
{-# INLINE read #-}

-- ---------------
-- repeat
-- ---------------

repeat :: a -> Stream (Of a) m r
repeat a = loop where loop = Step (a :> loop)
{-# INLINE repeat #-}

repeatM :: Monad m => m a -> Stream (Of a) m r
repeatM ma = loop where
  loop = Delay $ do 
    a <- ma 
    return (Step (a :> loop))
{-# INLINEABLE repeatM #-}

-- ---------------
-- replicate 
-- ---------------

replicate :: Monad m => Int -> a -> Stream (Of a) m ()
replicate n a = loop n where
  loop 0 = Return ()
  loop m = Step (a :> loop (m-1))
{-# INLINEABLE replicate #-}

-- | Repeat an action, streaming the results.
replicateM :: Monad m => Int -> m a -> Stream (Of a) m ()
replicateM n ma = loop n where 
  loop 0 = Return ()
  loop n = Delay $ do 
    a <- ma 
    return (Step $ a :> loop (n-1))
{-# INLINEABLE replicateM #-}


{-| Strict left scan, streaming, e.g. successive partial results.

> Control.Foldl.purely scan :: Monad m => Fold a b -> Stream (Of a) m r -> Stream (Of b) m r
-}
scan :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r -> Stream (Of b) m r
scan step begin done = loop begin
  where
    loop !x stream = do 
      yield (done x)
      case stream of 
        Return r -> Return r
        Delay m  -> Delay $ liftM (loop x) m
        Step (a :> rest) -> do
          let x' = step x a
          loop x' rest
{-# INLINABLE scan #-}

{-| Strict, monadic left scan

> Control.Foldl.impurely scanM :: Monad m => FoldM a m b -> Stream (Of a) m r -> Stream (Of b) m r
-}
scanM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
scanM step begin done str = do
    x <- lift begin
    loop x str
  where
    loop !x stream = do 
      b <- lift (done x)
      yield b
      case stream of 
        Return r -> Return r
        Delay m  -> Delay $ liftM (loop x) m
        Step (a :> rest) -> do
          x' <- lift $ step x a
          loop x' rest
{-# INLINABLE scanM #-}

-- ---------------
-- sequence
-- ---------------

-- | Like the 'Data.List.sequence' but streaming. The result type is a
-- stream of a\'s, but is not accumulated; the effects of the elements
-- of the original stream are interleaved in the resulting stream.

sequence :: Monad m => Stream (Of (m a)) m r -> Stream (Of a) m r
sequence = loop where
  loop stream = case stream of
    Return r          -> Return r
    Delay m           -> Delay $ liftM loop m
    Step (ma :> rest) -> Delay $ do
      a <- ma
      return (Step (a :> loop rest))
{-# INLINEABLE sequence #-}

-- ---------------
-- show
-- ---------------

show :: (Monad m, Show a) => Stream (Of a) m r -> Stream (Of String) m r
show = map Prelude.show
{-# INLINE show #-}
-- ---------------
-- sum 
-- ---------------

-- | Fold a 'Stream' of numbers into their sum
sum :: (Monad m, Num a) => Stream (Of a) m () -> m a
sum = fold (+) 0 id
{-# INLINE sum #-}

{-| Fold a 'Stream' of numbers into their sum with the return value

>  mapsFold sum' :: Stream (Stream (Of Int)) m r -> Stream (Of Int) m r
-}
sum' :: (Monad m, Num a) => Stream (Of a) m r -> m (a, r)
sum' = fold' (+) 0 id
{-# INLINE sum' #-}

-- ---------------
-- span
-- ---------------

-- | Stream elements until one fails the condition, return the rest.
span :: Monad m => (a -> Bool) -> Stream (Of a) m r 
      -> Stream (Of a) m (Stream (Of a) m r)
span pred = loop where
  loop str = case str of 
    Return r         -> Return (Return r)
    Delay m          -> Delay $ liftM loop m
    Step (a :> rest) -> if pred a 
      then Step (a :> loop rest)
      else Return (Step (a :> rest))
{-# INLINEABLE span #-}


-- ---------------
-- take
-- ---------------

-- | End stream after n elements; the original return value is lost.
-- 'splitAt' preserves this information. Note the function is functor-general.

take :: (Monad m, Functor f) => Int -> Stream f m r -> Stream f m ()
take = loop where
  loop n p = when (n > 0) $
    case p of Step fas -> Step (fmap (loop (n-1)) fas)
              Delay m -> Delay (liftM (loop n) m)
              Return r -> Return ()
{-# INLINEABLE take #-}

-- ---------------
-- takeWhile
-- ---------------

-- | End stream when an element fails a condition; the original return value is lost
-- 'span' preserves this information.
takeWhile :: Monad m => (a -> Bool) -> Stream (Of a) m r -> Stream (Of a) m ()
takeWhile pred = loop where
  loop str = case str of 
    Step (a :> as) -> when (pred a) (Step (a :> loop as))
    Delay m              -> Delay (liftM loop m)
    Return r              -> Return ()
{-# INLINEABLE takeWhile #-}



-- | Convert a pure 'Stream (Of a) into a list of a
toList :: Stream (Of a) Identity () -> [a]
toList = loop
  where
    loop stream = case stream of
       Return _                 -> []
       Delay (Identity stream') -> loop stream'
       Step (a :> rest)         -> a : loop rest
{-# INLINABLE toList #-}

{-| Convert an effectful 'Stream (Of a)' into a list of a

    Note: 'toListM' is not an idiomatic use of @pipes@, but I provide it for
    simple testing purposes.  Idiomatic @pipes@ style consumes the elements
    immediately as they are generated instead of loading all elements into
    memory.
-}
toListM :: Monad m => Stream (Of a) m () -> m [a]
toListM = fold (\diff a ls -> diff (a: ls)) id (\diff -> diff [])
{-# INLINE toListM #-}


{-| Convert an effectful 'Stream' into a list alongside the return value

    Note: 'toListM'' is not an idiomatic use of @streaming@, but I provide it for
    simple testing purposes.  Idiomatic @streaming@ style, like idiomatic @pipes@ style
    consumes the elements as they are generated instead of loading all elements into
    memory.

>  mapsFold toListM' :: Stream (Stream (Of a)) m r -> Stream (Of [a]) m 
-}
toListM' :: Monad m => Stream (Of a) m r -> m ([a], r)
toListM' = fold' (\diff a ls -> diff (a: ls)) id (\diff -> diff [])
{-# INLINE toListM' #-}


-- ---------------------------------------
-- yield
-- ---------------------------------------

-- | Yield an individual item
yield :: Monad m => a -> Stream (Of a) m ()
yield a = Step (a :> Return ())
{-# INLINE yield #-}

-- | Zip two 'Streams's 
zip :: Monad m
    => (Stream (Of a) m r)
    -> (Stream (Of b) m r)
    -> (Stream (Of (a,b)) m r)
zip = zipWith (,)
{-# INLINE zip #-}

-- | Zip two 'Streams's using the provided combining function
zipWith :: Monad m
    => (a -> b -> c)
    -> (Stream (Of a) m r)
    -> (Stream (Of b) m r)
    -> (Stream (Of c) m r)
zipWith f = loop
  where
    loop str0 str1 = case str0 of
      Return r          -> Return r
      Delay m           -> Delay $ liftM (\str -> loop str str1) m 
      Step (a :> rest0) -> case str1 of
        Return r          -> Return r
        Delay m           -> Delay $ liftM (loop str0) m
        Step (b :> rest1) -> Step (f a b :>loop rest0 rest1)
{-# INLINABLE zipWith #-}

-- ---------------------------------------
-- IO fripperies copped from Pipes.Prelude
-- ---------------------------------------

-- | repeatedly stream lines as 'String' from stdin
stdinLn :: MonadIO m => Stream (Of String) m ()
stdinLn = fromHandle IO.stdin
{-# INLINABLE stdinLn #-}

-- | 'read' values from 'IO.stdin', ignoring failed parses
readLn :: (MonadIO m, Read a) => Stream (Of a) m ()
readLn = for stdinLn $ \str -> case readMaybe str of 
  Nothing -> return ()
  Just n  -> yield n
{-# INLINABLE readLn #-}

{-| Read 'String's from a 'IO.Handle' using 'IO.hGetLine'

    Terminates on end of input
-}
fromHandle :: MonadIO m => IO.Handle -> Stream (Of String) m ()
fromHandle h = go
  where
    go = do
        eof <- liftIO $ IO.hIsEOF h
        unless eof $ do
            str <- liftIO $ IO.hGetLine h
            yield str
            go
{-# INLINABLE fromHandle #-}     

toHandle :: MonadIO m => IO.Handle -> Stream (Of String) m r -> m r
toHandle handle = loop where
  loop str = case str of
    Return r         -> return r
    Delay m          -> m >>= loop 
    Step (s :> rest) -> do 
      liftIO $ IO.hPutStrLn handle s
      loop rest
{-# INLINABLE toHandle #-} 

print :: (MonadIO m, Show a) => Stream (Of a) m r -> m r
print = loop where
  loop stream = case stream of 
    Return r         -> return r 
    Delay m          -> m >>= loop
    Step (a :> rest) -> do 
      liftIO (Prelude.print a)
      loop rest

-- | Evaluate all values flowing downstream to WHNF
seq :: Monad m => Stream (Of a) m r -> Stream (Of a) m r 
seq str = for str $ \a -> yield $! a
{-# INLINABLE seq #-}

{-| Write 'String's to 'IO.stdout' using 'putStrLn'

    Unlike 'toHandle', 'stdoutLn' gracefully terminates on a broken output pipe
-}
stdoutLn :: MonadIO m => Stream (Of String) m () -> m ()
stdoutLn = loop
  where
    loop stream = case stream of 
      Return _         -> return () 
      Delay m          -> m >>= loop
      Step (s :> rest) -> do
        x   <- liftIO $ try (putStrLn s)
        case x of
           Left (G.IOError { G.ioe_type  = G.ResourceVanished
                           , G.ioe_errno = Just ioe })
                | Errno ioe == ePIPE
                    -> return ()
           Left  e  -> liftIO (throwIO e)
           Right () -> loop rest
{-# INLINABLE stdoutLn #-}


{-| Write 'String's to 'IO.stdout' using 'putStrLn'

    This does not handle a broken output pipe, but has a polymorphic return
    value
-}

stdoutLn' :: MonadIO m => Stream (Of String) m r -> m r
stdoutLn' = loop where 
  loop stream = case stream of 
    Return r         -> return r 
    Delay m          -> m >>= loop
    Step (s :> rest) -> liftIO (putStrLn s) >> loop rest
{-# INLINE stdoutLn' #-}


-- -- * Producers
-- -- $producers
--   stdinLn  -- 
-- , readLn -- 
-- , fromHandle -- 
-- , repeatM -- 
-- , replicateM --
--
-- -- * Consumers
-- -- $consumers
-- , stdoutLn --
-- , stdoutLn' --
-- , mapM_ --
-- , print -- 
-- , toHandle --
-- , drain --
--
-- -- * Pipes
-- -- $pipes
-- , map -- 
-- , mapM --
-- , sequence -- 
-- , mapFoldable -- 
-- , filter --
-- , filterM --
-- , take --
-- , takeWhile --
-- , takeWhile' --
-- , drop --
-- , dropWhile -- 
-- , concat --
-- , elemIndices
-- , findIndices
-- , scan --
-- , scanM --
-- , chain --
-- , read --
-- , show -- 
-- , seq --
--
-- -- * Folds
-- -- $folds
-- , fold --
-- , fold' --
-- , foldM --
-- , foldM' --
-- , all
-- , any
-- , and
-- , or
-- , elem
-- , notElem
-- , find
-- , findIndex
-- , head
-- , index
-- , last
-- , length
-- , maximum
-- , minimum
-- , null
-- , sum --
-- , product --
-- , toList --
-- , toListM --
-- , toListM' --
--
-- -- * Zips
-- , zip --
-- , zipWith --
--