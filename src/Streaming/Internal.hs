{-# LANGUAGE RankNTypes, StandaloneDeriving,DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE UndecidableInstances, CPP, FlexibleInstances, MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables, GADTs #-}
{-# LANGUAGE RebindableSyntax, PartialTypeSignatures, InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Streaming.Internal (
    -- * The free monad transformer
    -- $stream
    Stream (..)

    -- * Introducing a stream
    , unfold
    , replicates
    , repeats
    , repeatsM
    , effect
    , wrap
    , yields
    , streamBuild
    , cycles
    , delays
    , never
    , untilJust

    -- * Eliminating a stream
    , intercalates
    , concats
    --, iterT
    --, iterTM
    , destroy
    , streamFold

    -- * Inspecting a stream wrap by wrap
    , inspect

    -- * Transforming streams
    , maps
    , mapsM
    , decompose
    , mapsM_
    , run
    , distribute
    --, groups
--    , groupInL

    -- *  Splitting streams
    , chunksOf
    , splitsAt
    --, takes
    --, cutoff
    -- , period
    -- , periods

    -- * Zipping and unzipping streams
    --, zipsWith
    --, zips
    , unzips
    --, interleaves
    , separate
    , unseparate


    -- * Assorted Data.Functor.x help

    , switch

    -- * ResourceT and MonadMask help

    --, bracketStream
    --, bracket

    -- *  For use in implementation
    , unexposed
    , hoistExposed
    , mapsExposed
    , mapsMExposed
    , destroyExposed

   ) where

import Control.Monad.LMonad
import Data.Functor.LFunctor
import Control.Monad.Trans.LClass
import Control.Monad.IO.LClass
import Control.Applicative.LApplicative
import Control.Monad.Morph.LMorph
import Data.Data ( Data, Typeable )
import Prelude hiding (splitAt, (>>), (>>=), return, fmap, fail, pure, (<$>))
import Data.Functor.Compose
import Data.Functor.Sum
import Control.Concurrent (threadDelay)
import Data.Linear (liftUnit)

{- $stream

    The 'Stream' data type is equivalent to @FreeT@ and can represent any effectful
    succession of steps, where the form of the steps or 'commands' is
    specified by the first (functor) parameter.

> data Stream f m r = Step !(f (Stream f m r)) | Effect (m (Stream f m r)) | Return r

    The /producer/ concept uses the simple functor @ (a,_) @ \- or the stricter
    @ Of a _ @. Then the news at each step or layer is just: an individual item of type @a@.
    Since @Stream (Of a) m r@ is equivalent to @Pipe.Producer a m r@, much of
    the @pipes@ @Prelude@ can easily be mirrored in a @streaming@ @Prelude@. Similarly,
    a simple @Consumer a m r@ or @Parser a m r@ concept arises when the base functor is
    @ (a -> _) @ . @Stream ((->) input) m result@ consumes @input@ until it returns a
    @result@.

    To avoid breaking reasoning principles, the constructors
    should not be used directly. A pattern-match should go by way of 'inspect' \
    \- or, in the producer case, 'Streaming.Prelude.next'
    The constructors are exported by the 'Internal' module.
-}
data Stream f m r = Step !(f (Stream f m r))
                  | Effect (m (Stream f m r))
                  | Return r
#if __GLASGOW_HASKELL__ >= 710
                  deriving (Typeable)
#endif
deriving instance (Show r, Show (m (Stream f m r))
                  , Show (f (Stream f m r))) => Show (Stream f m r)
deriving instance (Eq r, Eq (m (Stream f m r))
                  , Eq (f (Stream f m r))) => Eq (Stream f m r)
#if __GLASGOW_HASKELL__ >= 710
deriving instance (Typeable f, Typeable m, Data r, Data (m (Stream f m r))
                  , Data (f (Stream f m r))) => Data (Stream f m r)
#endif

instance (LFunctor f, LMonad m) => LFunctor (Stream f m) where
  fmap f (Return r)  = Return (f r)
  fmap f (Effect m)  = Effect (do {stream' <- m; return (fmap f stream')})
  fmap f (Step fstr) = Step (fmap (fmap f) fstr)
  {-# INLINABLE fmap #-}
  (<$) = fmap . liftUnit
  {-# INLINABLE (<$) #-}

instance (LFunctor f, LMonad m) => LMonad (Stream f m) where
  return = Return
  {-# INLINE return #-}
  (>>) :: Stream f m () ⊸ Stream f m r ⊸ Stream f m r
  stream1 >> stream2 = stream1 >>= \() -> stream2
  {-# INLINABLE (>>) #-}
  -- (>>=) = _bind
  -- {-#INLINE (>>=) #-}
  --
  (>>=) :: Stream f m a ⊸ (a ⊸ Stream f m b) ⊸ Stream f m b
  stream >>= f = loop f stream where
    loop :: (_ ⊸ _) ⊸ Stream f m _ ⊸ Stream f m _
    loop fn (Return r)  = fn r
    loop fn (Effect m)  = Effect $ fmap (loop fn) m
    loop fn (Step fstr) = Step $ fmap (loop fn) fstr
  {-# INLINABLE (>>=) #-}

  fail = lift . fail
  {-#INLINE fail #-}


-- _bind
--     :: (Functor f, Monad m)
--     => Stream f m r
--     -> (r -> Stream f m s)
--     -> Stream f m s
-- _bind p0 f = go p0 where
--     go p = case p of
--       Step fstr  -> Step (fmap go fstr)
--       Effect m   -> Effect (m >>= \s -> return (go s))
--       Return r  -> f r
-- {-#INLINABLE _bind #-}
--
-- see https://github.com/Gabriel439/Haskell-Pipes-Library/pull/163
-- for a plan to delay inlining and manage interaction with other operations.

-- {-# RULES
    -- "_bind (Step    fstr) f" forall  fstr f .
    --     _bind (Step fstr) f = Step (fmap (\p -> _bind p f) fstr);
    -- "_bind (Effect      m) f" forall m    f .
    --     _bind (Effect   m) f = Effect (m >>= \p -> return (_bind p f));
    -- "_bind (Return     r) f" forall r    f .
    --     _bind (Return  r) f = f r;
--  #-}

instance (LFunctor f, LMonad m) => LApplicative (Stream f m) where
  pure = Return
  {-# INLINE pure #-}
  streamf <*> streamx = do
    f <- streamf
    x <- streamx
    return (f x)
  {-# INLINE (<*>) #-}

{- | The 'Alternative' instance glues streams together stepwise.

> empty = never
> (<|>) = zipsWith (liftA2 (,))

   See also 'never', 'untilJust' and 'delays'
-}
--instance (Applicative f, Monad m) => Alternative (Stream f m) where
--  empty = never
--  {-#INLINE empty #-}
--
--  str <|> str' = zipsWith (liftA2 (,)) str str'
--  {-#INLINE (<|>) #-}
--
--instance (Functor f, Monad m, Monoid w) => Monoid (Stream f m w) where
--  mempty = return mempty
--  {-#INLINE mempty #-}
--  mappend a b = a >>= \w -> fmap (w <>) b
--  {-#INLINE mappend #-}
--
--instance (Applicative f, Monad m) => MonadPlus (Stream f m) where
--  mzero = empty
--  mplus = (<|>)

instance LFunctor f => LMonadTrans (Stream f) where
  lift = Effect . fmap Return
  {-# INLINE lift #-}

instance LFunctor f => LMFunctor (Stream f) where
  hoist trans = loop where
    loop :: Stream f _ _ ⊸ Stream f _ _
    loop (Return r) = Return r
    loop (Effect m) = Effect $ trans $ fmap loop m
    loop (Step   f) = Step $ fmap loop f
  {-# INLINABLE hoist #-}

--instance Functor f => MMonad (Stream f) where
--  embed phi = loop where
--    loop stream = case stream of
--      Return r -> Return r
--      Effect  m -> phi m >>= loop
--      Step   f -> Step (fmap loop f)
--  {-# INLINABLE embed #-}

instance (LMonadIO m, LFunctor f) => LMonadIO (Stream f m) where
  liftIO = Effect . fmap Return . liftIO
  {-# INLINE liftIO #-}

--instance (MonadBase b m, Functor f) => MonadBase b (Stream f m) where
--  liftBase  = effect . fmap return . liftBase
--  {-#INLINE liftBase #-}
--
--instance (MonadThrow m, Functor f) => MonadThrow (Stream f m) where
--  throwM = lift . throwM
--  {-#INLINE throwM #-}
--
--instance (MonadCatch m, Functor f) => MonadCatch (Stream f m) where
--  catch str f = go str
--    where
--    go p = case p of
--      Step f      -> Step (fmap go f)
--      Return  r   -> Return r
--      Effect  m   -> Effect (catch (do
--          p' <- m
--          return (go p'))
--       (\e -> return (f e)) )
--  {-#INLINABLE catch #-}
--
---- The materials for the MonadMask instance are entirely lifted from pipes-safe
---- following remarks of Oliver Charles.
--data Restore m = Unmasked | Masked (forall x . m x -> m x)
--
--liftMask
--    :: forall m f r a . (MonadIO m, MonadCatch m, f ~ (Of a))
--    => (forall s . ((forall x . m x -> m x) -> m s) -> m s)
--    -> ((forall x . Stream f m x -> Stream f m x)
--        -> Stream f m r)
--    -> Stream f m r
--liftMask maskVariant k = do
--    ioref <- liftIO $ newIORef Unmasked
--
--    let -- mask adjacent actions in base monad
--        loop :: Stream f m r -> Stream f m r
--        loop (Step f)   = Step (fmap loop f)
--        loop (Return r) = Return r
--        loop (Effect m) = Effect $ maskVariant $ \unmaskVariant -> do
--            -- stash base's unmask and merge action
--            liftIO $ writeIORef ioref $ Masked unmaskVariant
--            m >>= chunk >>= return . loop
--
--        -- unmask adjacent actions in base monad
--        unmask :: forall q. Stream f m q -> Stream f m q
--        unmask (Step f)   = Step (fmap unmask f)
--        unmask (Return q) = Return q
--        unmask (Effect m) = Effect $ do
--            -- retrieve base's unmask and apply to merged action
--            Masked unmaskVariant <- liftIO $ readIORef ioref
--            unmaskVariant (m >>= chunk >>= return . unmask)
--
--        -- merge adjacent actions in base monad
--        chunk :: forall s. Stream f m s -> m (Stream f m s)
--        chunk (Effect m) = m >>= chunk
--        chunk s          = return s
--
--    loop $ k unmask
--
--instance (MonadMask m, MonadIO m, f ~ (Of a)) => MonadMask (Stream f m) where
--    mask                = liftMask mask
--    uninterruptibleMask = liftMask uninterruptibleMask
--
--instance (MonadResource m, Functor f) => MonadResource (Stream f m) where
--  liftResourceT = lift . liftResourceT
--  {-#INLINE liftResourceT #-}
--
--
--instance (Functor f, MonadReader r m) => MonadReader r (Stream f m) where
--  ask = lift ask
--  {-# INLINE ask #-}
--  local f = hoist (local f)
--  {-# INLINE local #-}
--
--instance (Functor f, MonadState s m) => MonadState s (Stream f m) where
--  get = lift get
--  {-# INLINE get #-}
--  put = lift . put
--  {-# INLINE put #-}
-- #if MIN_VERSION_mtl(2,1,1)
--  state f = lift (state f)
--  {-# INLINE state #-}
-- #endif
--
--instance (Functor f, MonadError e m) => MonadError e (Stream f m) where
--  throwError = lift . throwError
--  {-# INLINE throwError #-}
--  str `catchError` f = loop str where
--    loop str = case str of
--      Return r -> Return r
--      Effect m -> Effect $ liftM loop m `catchError` (return . f)
--      Step f -> Step (fmap loop f)
--  {-# INLINABLE catchError #-}

--bracketStream :: (LFunctor f, MonadResource m) =>
--       IO a -> (a -> IO ()) -> (a -> Stream f m b) -> Stream f m b
--bracketStream alloc free inside = do
--        (key, seed) <- lift (allocate alloc free)
--        clean key (inside seed)
--  where
--    clean key = loop where
--      loop str = case str of
--        Return r -> Effect (release key >> return (Return r))
--        Effect m -> Effect (liftM loop m)
--        Step f   -> Step (fmap loop f)
--{-#INLINABLE bracketStream #-}
--
--
--bracket
--  :: (MonadIO m, MonadMask m, MonadResource m)
--  => m b -> (b -> IO ()) -> (b -> Stream (Of a) m r) -> Stream (Of a) m r
--bracket before after action = mask $ \restore -> do
--    h <- lift before
--    r <- restore (action h) `onException` after h
--    liftIO (after h)
--    return r
--
--onException :: (Functor f, MonadResource m) => Stream f m a -> IO () -> Stream f m a
--m1 `onException` io = do
--  key <- lift (register io)
--  clean key m1
--  where
--    clean key = loop
--      where
--        loop str =
--          case str of
--            Return r -> Effect (unprotect key >> return (Return r))
--            Effect m -> Effect (fmap loop m)
--            Step f -> Step (fmap loop f)

{-| Map a stream directly to its church encoding; compare @Data.List.foldr@
-}
destroy :: (LFunctor f, LMonad m) =>
     Stream f m r ⊸ (f b ⊸ b) -> (m b ⊸ b) -> (r ⊸ b) ⊸  b
destroy stream0 construct eff done = loop done stream0 where
  loop :: (r ⊸ b) ⊸ Stream f m r ⊸ _
  loop endFn (Return r) = endFn r
  loop endFn (Effect m) = eff $ fmap (loop endFn) m
  loop endFn (Step fs)  = construct $ fmap (loop endFn) fs
{-# INLINABLE destroy #-}


{-| 'streamFold' reorders the arguments of 'destroy' to be more akin
    to @foldr@  It is more convenient to query in ghci to figure out
    what kind of \'algebra\' you need to write.

>>> :t streamFold return join
(Monad m, Functor f) =>
     (f (m a) -> m a) -> Stream f m a -> m a        -- iterT

>>> :t streamFold return (join . lift)
(Monad m, Monad (t m), Functor f, MonadTrans t) =>
     (f (t m a) -> t m a) -> Stream f m a -> t m a  -- iterTM

>>> :t streamFold return effect
(Monad m, Functor f, Functor g) =>
     (f (Stream g m r) -> Stream g m r) -> Stream f m r -> Stream g m r

>>> :t \f -> streamFold return effect (wrap . f)
(Monad m, Functor f, Functor g) =>
     (f (Stream g m a) -> g (Stream g m a))
     -> Stream f m a -> Stream g m a                 -- maps

>>> :t \f -> streamFold return effect (effect . liftM wrap . f)
(Monad m, Functor f, Functor g) =>
     (f (Stream g m a) -> m (g (Stream g m a)))
     -> Stream f m a -> Stream g m a                 -- mapped

-}
streamFold :: (LFunctor f, LMonad m)
           => (r ⊸ b) ⊸ (m b ⊸ b) -> (f b ⊸ b) -> Stream f m r ⊸ b
streamFold done eff construct stream  = destroy stream construct eff done
{-#INLINE streamFold #-}

{- | Reflect a church-encoded stream; cp. @GHC.Exts.build@

> streamFold return_ effect_ step_ (streamBuild psi)  = psi return_ effect_ step_
-}
streamBuild
  :: (forall b . (r -> b) -> (m b -> b) -> (f b -> b) ->  b) ⊸ Stream f m r
streamBuild phi = phi Return Effect Step
{-# INLINE streamBuild #-}


{-| Inspect the first stage of a freely layered sequence.
    Compare @Pipes.next@ and the replica @Streaming.Prelude.next@.
    This is the 'uncons' for the general 'unfold'.

> unfold inspect = id
> Streaming.Prelude.unfoldr StreamingPrelude.next = id
-}
inspect :: forall f m r. (LFunctor f, LMonad m)
        => Stream f m r ⊸ m (Either r (f (Stream f m r)))
inspect (Return r) = return $ Left r
inspect (Effect m) = m >>= inspect
inspect (Step fs)  = return $ Right fs
{-# INLINABLE inspect #-}

{-| Build a @Stream@ by unfolding steps starting from a seed. See also
    the specialized 'Streaming.Prelude.unfoldr' in the prelude.

> unfold inspect = id -- modulo the quotient we work with
> unfold Pipes.next :: Monad m => Producer a m r -> Stream ((,) a) m r
> unfold (curry (:>) . Pipes.next) :: Monad m => Producer a m r -> Stream (Of a) m r

-}
unfold :: (LMonad m, LFunctor f)
        => (s -> m (Either r (f s))) -> s -> Stream f m r
unfold step = loop where
  loop s0 = Effect $ do
    e <- step s0
    case e of
      Left r -> return (Return r)
      Right fs -> return (Step (fmap loop fs))
{-# INLINABLE unfold #-}


{- | Map layers of one functor to another with a transformation. Compare
     hoist, which has a similar effect on the 'monadic' parameter.

> maps id = id
> maps f . maps g = maps (f . g)

-}
maps :: forall m f g r. (LMonad m, LFunctor f)
     => (forall x . f x ⊸ g x) -> Stream f m r ⊸ Stream g m r
maps phi = loop where
  loop :: Stream f m r ⊸ Stream g m r
  loop (Return r) = Return r
  loop (Effect m) = Effect $ fmap loop m
  loop (Step f)   = Step $ phi $ fmap loop f
{-# INLINABLE maps #-}


{- | Map layers of one functor to another with a transformation involving the base monad
     @maps@ is more fundamental than @mapsM@, which is best understood as a convenience
     for effecting this frequent composition:

> mapsM phi = decompose . maps (Compose . phi)

     The streaming prelude exports the same function under the better name @mapped@,
     which overlaps with the lens libraries.

-}
mapsM :: (LMonad m, LFunctor f)
      => (forall x . f x ⊸ m (g x)) -> Stream f m r ⊸ Stream g m r
mapsM phi = loop where
  loop :: Stream _ m r ⊸ Stream _ m r
  loop (Return r) = Return r
  loop (Effect m) = Effect $ fmap loop m
  loop (Step f)   = Effect $ fmap Step $ phi $ fmap loop f
{-# INLINABLE mapsM #-}


{-| Rearrange a succession of layers of the form @Compose m (f x)@.

   we could as well define @decompose@ by @mapsM@:

> decompose = mapped getCompose

  but @mapped@ is best understood as:

> mapped phi = decompose . maps (Compose . phi)

  since @maps@ and @hoist@ are the really fundamental operations that preserve the
  shape of the stream:

> maps  :: (Monad m, Functor f) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
> hoist :: (Monad m, Functor f) => (forall a. m a -> n a) -> Stream f m r -> Stream f n r

-}
decompose :: forall m f r. (LMonad m, LFunctor f)
          => Stream (Compose m f) m r ⊸ Stream f m r
decompose = loop where
  loop :: Stream (Compose m f) m r ⊸ Stream f m r
  loop (Return r) = Return r
  loop (Effect m) = Effect $ fmap loop m
  loop (Step (Compose mstr)) = Effect $ do
    str <- mstr
    return $ Step $ fmap loop str

{-| Run the effects in a stream that merely layers effects.
-}
run :: forall m r. LMonad m => Stream m m r ⊸ m r
run = loop where
  loop :: Stream m m r ⊸ m r
  loop (Return r)   = return r
  loop (Effect m)   = m >>= loop
  loop (Step mrest) = mrest >>= loop
{-# INLINABLE run #-}


{-| Map each layer to an effect, and run them all.
-}
mapsM_ :: (LFunctor f, LMonad m) => (forall x . f x ⊸ m x) -> Stream f m r ⊸ m r
mapsM_ f = run . maps f
{-# INLINE mapsM_ #-}


{-| Interpolate a layer at each segment. This specializes to e.g.

> intercalates :: (LMonad m, LFunctor f) => Stream f m () -> Stream (Stream f m) m r  ⊸ Stream f m r
-}
intercalates :: forall m t x r. (LMonad m, LMonad (t m), LMonadTrans t)
             => t m x -> Stream (t m) m r ⊸ t m r
intercalates sep = go0
  where
    go0 :: Stream (t m) m r ⊸ t m r
    go0 (Return  r) = return r
    go0 (Effect  m) = lift m >>= go0
    go0 (Step fstr) = do
      f' <- fstr
      go1 f'
    go1 :: Stream (t m) m r ⊸ t m r
    go1 (Return  r) = return r
    go1 (Effect  m) = lift m >>= go1
    go1 (Step fstr) = do
      _ <- sep
      f' <- fstr
      go1 f'
{-# INLINABLE intercalates #-}

{-| Specialized fold following the usage of @Control.Monad.Trans.Free@

> iterTM alg = streamFold return (join . lift)
-}
--iterTM ::
--  (Functor f, Monad m, MonadTrans t,
--   Monad (t m)) =>
--  (f (t m a) -> t m a) -> Stream f m a -> t m a
--iterTM out stream = destroyExposed stream out (join . lift) return
--{-# INLINE iterTM #-}
--
--{-| Specialized fold following the usage of @Control.Monad.Trans.Free@
--
-- > iterT alg = streamFold return join alg
---}
--iterT ::
--  (Functor f, Monad m) => (f (m a) -> m a) -> Stream f m a -> m a
--iterT out stream = destroyExposed stream out join return
--{-# INLINE iterT #-}

{-| Dissolves the segmentation into layers of @Stream f m@ layers.

-}
concats :: forall f m r. (LMonad m, LFunctor f) => Stream (Stream f m) m r ⊸ Stream f m r
concats  = loop where
  loop :: Stream (Stream f m) m r ⊸ Stream f m r
  loop (Return r) = return r
  loop (Effect m) = join $ lift $ fmap loop m
  loop (Step fs)  = join $ fmap loop fs
{-# INLINE concats #-}

{-| Split a succession of layers after some number, returning a streaming or
    effectful pair.

>>> rest <- S.print $ S.splitAt 1 $ each [1..3]
1
>>> S.print rest
2
3

> splitAt 0 = return
> splitAt n >=> splitAt m = splitAt (m+n)

    Thus, e.g.

>>> rest <- S.print $ splitsAt 2 >=> splitsAt 2 $ each [1..5]
1
2
3
4
>>> S.print rest
5

-}
splitsAt :: forall f m r. (LMonad m, LFunctor f)
         => Int -> Stream f m r ⊸ Stream f m (Stream f m r)
splitsAt  = loop where
  loop :: Int -> Stream f m r ⊸ Stream f m (Stream f m r)
  loop !n stream | n <= 0 = Return stream
  loop _ (Return r) = Return (Return r)
  loop !n (Effect m) = Effect $ fmap (loop n) m
  loop !n (Step  fs) = case n of
    0 -> Return $ Step fs
    _ -> Step $ loop (n-1) <$> fs
{-# INLINABLE splitsAt #-}

{- Functor-general take.

   @takes 3@ can take three individual values

>>> S.print $ takes 3 $ each [1..]
1
2
3


    or three sub-streams

>>> S.print $ mapped S.toList $ takes 3 $ chunksOf 2 $ each [1..]
[1,2]
[3,4]
[5,6]

   Or, using 'Data.ByteString.Streaming.Char' (here called @Q@),
   three byte streams.

>>> Q.stdout $ Q.unlines $ takes 3 $ Q.lines $ Q.chunk "a\nb\nc\nd\ne\nf"
a
b
c

-}
--takes :: (LMonad m, LFunctor f) => Int -> Stream f m r ⊸ Stream f m ()
--takes n = void . splitsAt n
--{-# INLINE takes #-}

{-| Break a stream into substreams each with n functorial layers.

>>>  S.print $ mapped S.sum $ chunksOf 2 $ each [1,1,1,1,1]
2
2
1
-}
chunksOf :: forall f m r. (LMonad m, LFunctor f) => Int -> Stream f m r ⊸ Stream (Stream f m) m r
chunksOf n0 = loop where
  loop :: Stream f m r ⊸ Stream (Stream f m) m r
  loop (Return r) = Return r
  loop (Effect m) = Effect $ fmap loop m
  loop (Step  fs) = Step $ Step $ fmap (fmap loop . splitsAt (n0-1)) fs
{-# INLINABLE chunksOf #-}

{- | Make it possible to \'run\' the underlying transformed monad.
-}
distribute :: forall m f t r. (LMonad m, LFunctor f, LMonadTrans t,
                         LMFunctor t, LMonad (t (Stream f m)))
           => Stream f (t m) r ⊸ t (Stream f m) r
distribute = loop where
  loop :: Stream f (t m) r ⊸ t (Stream f m) r
  loop (Return r)     = lift $ Return r
  loop (Effect tmstr) = hoist lift tmstr >>= loop
  loop (Step   fstr)  = join $ lift $ Step $ fmap (Return . loop) fstr
{-#INLINABLE distribute #-}

-- | Repeat a functorial layer (a \"command\" or \"instruction\") forever.
repeats :: forall f m r. (LMonad m, LFunctor f) => f () -> Stream f m r
repeats f = loop where
  loop :: Stream f m r
  loop = Effect $ return $ Step $ fmap (liftUnit loop) f

-- | Repeat an effect containing a functorial layer, command or instruction forever.
repeatsM :: forall f m r. (LMonad m, LFunctor f) => m (f ()) -> Stream f m r
repeatsM mf = loop where
  loop :: Stream f m r
  loop = Effect $ do
     f <- mf
     return $ Step $ fmap (liftUnit loop) f

{- | Repeat a functorial layer, command or instruction a fixed number of times.

> replicates n = takes n . repeats
-}
replicates :: (LMonad m, LFunctor f) => Int -> f () -> Stream f m ()
replicates 0 _ = return ()
replicates n f = Effect $ return $ Step $ fmap (\() -> replicates (n-1) f) f

{-| Construct an infinite stream by cycling a finite one

> cycles = forever

>>>
-}

cycles :: (LMonad m, LFunctor f) => Stream f m () -> Stream f m r
cycles = forever


hoistExposed :: forall m m1 f r. (LFunctor f, LMonad m1)
             => (m1 (Stream f m r) ⊸ m (Stream f m r)) -> Stream f m1 r ⊸ Stream f m r
hoistExposed trans = loop where
  loop :: Stream f m1 r ⊸ Stream f m r
  loop (Return r) = Return r
  loop (Effect m) = Effect $ trans $ fmap loop m
  loop (Step  fs) = Step $ fmap loop fs

mapsExposed :: forall f g m r. (LMonad m, LFunctor f)
            => (forall x . f x ⊸ g x) -> Stream f m r ⊸ Stream g m r
mapsExposed phi = loop where
  loop :: Stream f m r ⊸ Stream g m r
  loop (Return r) = Return r
  loop (Effect m) = Effect $ fmap loop m
  loop (Step   f) = Step $ phi $ fmap loop f
{-# INLINABLE mapsExposed #-}

mapsMExposed :: forall f f1 m r. (LFunctor f1, LMonad m)
             => (f1 (Stream f m r) ⊸ m (f (Stream f m r))) -> Stream f1 m r ⊸ Stream f m r
mapsMExposed phi = loop where
  loop :: Stream f1 m r ⊸ Stream f m r
  loop (Return r) = Return r
  loop (Effect m) = Effect $ fmap loop m
  loop (Step  fs) = Effect $ fmap Step (phi (fmap loop fs))
{-# INLINABLE mapsMExposed #-}

--     Map a stream directly to its church encoding; compare @Data.List.foldr@
--     It permits distinctions that should be hidden, as can be seen from
--     e.g.
--
-- isPure stream = destroy (const True) (const False) (const True)
--
--     and similar nonsense.  The crucial
--     constraint is that the @m x -> x@ argument is an /Eilenberg-Moore algebra/.
--     See Atkey "Reasoning about Stream Processing with Effects"

destroyExposed :: forall f m r a. (LFunctor f, LMonad m)
               => Stream f m r ⊸ (f a ⊸ a) -> (m a ⊸ a) -> (r ⊸ a) -> a
destroyExposed stream0 construct eff done = loop stream0 where
  loop :: Stream f m r ⊸ a
  loop (Return r) = done r
  loop (Effect m) = eff $ fmap loop m
  loop (Step  fs) = construct $ fmap loop fs
{-# INLINABLE destroyExposed #-}


{-| This is akin to the @observe@ of @Pipes.Internal@ . It reeffects the layering
    in instances of @Stream f m r@ so that it replicates that of
    @FreeT@.

-}
unexposed :: forall f m r. (LFunctor f, LMonad m) => Stream f m r ⊸ Stream f m r
unexposed = Effect . loop where
  loop :: Stream f m r ⊸ m (Stream f m r)
  loop (Return r) = return $ Return r
  loop (Effect m) = m >>= loop
  loop (Step   f) = return $ Step $ fmap (Effect . loop) f
{-# INLINABLE unexposed #-}


{-| Wrap a new layer of a stream. So, e.g.

> S.cons :: Monad m => a -> Stream (Of a) m r -> Stream (Of a) m r
> S.cons a str = wrap (a :> str)

   and, recursively:

> S.each :: (Monad m, Foldable t) => t a -> Stream (Of a) m ()
> S.each = foldr (\a b -> wrap (a :> b)) (return ())

   The two operations

> wrap :: (Monad m, Functor f )   => f (Stream f m r) -> Stream f m r
> effect :: (Monad m, Functor f ) => m (Stream f m r) -> Stream f m r

   are fundamental. We can define the parallel operations @yields@ and @lift@ in
   terms of them

> yields :: (Monad m, Functor f )  => f r -> Stream f m r
> yields = wrap . fmap return
> lift ::  (Monad m, Functor f )   => m r -> Stream f m r
> lift = effect . fmap return

-}
wrap :: (LMonad m, LFunctor f ) => f (Stream f m r) ⊸ Stream f m r
wrap = Step
{-#INLINE wrap #-}


{- | Wrap an effect that returns a stream

> effect = join . lift

-}
effect :: (LMonad m, LFunctor f ) => m (Stream f m r) ⊸ Stream f m r
effect = Effect
{-#INLINE effect #-}


{-| @yields@ is like @lift@ for items in the streamed functor.
    It makes a singleton or one-layer succession.

> lift :: (Monad m, Functor f)    => m r -> Stream f m r
> yields ::  (Monad m, Functor f) => f r -> Stream f m r

    Viewed in another light, it is like a functor-general version of @yield@:

> S.yield a = yields (a :> ())

-}

yields :: (LMonad m, LFunctor f) => f r ⊸ Stream f m r
yields = Step . fmap Return
{-#INLINE yields #-}

{-| To preserve linearity we cannot discard the longer stream nor its end,
 - this data type represents the leftover after zipping.
 -
 - Same means the streams were of equal lengths, and contains the end results.
 - FstRest and SndRest means one stream was longer, and contains one end element
 - and one leftover stream.
 -}
--data ZipRest f g m r = Same (r, r)
--                     | FstRest (Stream f m r) r
--                     | SndRest r (Stream g m r)
--
--zipsWith :: (LMonad m, LFunctor f, LFunctor g, LFunctor h)
--  => (forall x y . f x ⊸ g y ⊸ h (x,y))
--  -> Stream f m r ⊸ Stream g m r ⊸ Stream h m (ZipRest f g m r)
--zipsWith phi s t = loop (s,t) where
--  loop :: (Stream _ _ _, Stream _ _ _) ⊸ Stream _ _ (ZipRest _ _ _ _)
--  loop (s1, s2) = Effect $ do
--    e1 <- inspect s1
--    e2 <- inspect s2
--    go e1 e2
--
--  go :: Either _ (_ (Stream _ _ _)) ⊸ Either _ (_ (Stream _ _ _))
--     ⊸ Stream _ _ (ZipRest _ _ _ _)
--  go (Left  r) (Left r') = Return $ Same (r, r')
--  go (Right s) (Left r)  = Return $ FstRest s r
--  go (Left  r) (Right s) = Return $ SndRest r s
--  go (Right fstr) (Right gstr) = Step $ fmap loop (phi fstr gstr)
--{-# INLINABLE zipsWith #-}
--
--zips :: (LMonad m, LFunctor f, LFunctor g)
--     => Stream f m r ⊸ Stream g m r ⊸ Stream (Compose f g) m (ZipRest f g m r)
--zips = zipsWith go where
--  go fx gy = Compose (fmap (\x -> fmap (\y -> (x,y)) gy) fx)
--{-# INLINE zips #-}



{-| Interleave functor layers, with the effects of the first preceding
    the effects of the second.

> interleaves = zipsWith (liftA2 (,))

>>> let paste = \a b -> interleaves (Q.lines a) (maps (Q.cons' '\t') (Q.lines b))
>>> Q.stdout $ Q.unlines $ paste "hello\nworld\n" "goodbye\nworld\n"
hello	goodbye
world	world

-}

--interleaves
--  :: (Monad m, Applicative h) =>
--     Stream h m r -> Stream h m r -> Stream h m r
--interleaves = zipsWith (liftA2 (,))
--{-# INLINE interleaves #-}


{-| Swap the order of functors in a sum of functors.

>>> S.toList $ S.print $ separate $ maps S.switch $ maps (S.distinguish (=='a')) $ S.each "banana"
'a'
'a'
'a'
"bnn" :> ()
>>> S.toList $ S.print $ separate $ maps (S.distinguish (=='a')) $ S.each "banana"
'b'
'n'
'n'
"aaa" :> ()
-}
switch :: Sum f g r ⊸ Sum g f r
switch (InL a) = InR a
switch (InR a) = InL a
{-#INLINE switch #-}



{-| Given a stream on a sum of functors, make it a stream on the left functor,
    with the streaming on the other functor as the governing monad. This is
    useful for acting on one or the other functor with a fold, leaving the
    other material for another treatment. It generalizes
    'Data.Either.partitionEithers', but actually streams properly.

>>> let odd_even = S.maps (S.distinguish even) $ S.each [1..10::Int]
>>> :t separate odd_even
separate odd_even
  :: Monad m => Stream (Of Int) (Stream (Of Int) m) ()

    Now, for example, it is convenient to fold on the left and right values separately:

>>>  S.toList $ S.toList $ separate odd_even
[2,4,6,8,10] :> ([1,3,5,7,9] :> ())


   Or we can write them to separate files or whatever:

>>> runResourceT $ S.writeFile "even.txt" . S.show $ S.writeFile "odd.txt" . S.show $ S.separate odd_even
>>> :! cat even.txt
2
4
6
8
10
>>> :! cat odd.txt
1
3
5
7
9

   Of course, in the special case of @Stream (Of a) m r@, we can achieve the above
   effects more simply by using 'Streaming.Prelude.copy'

>>> S.toList . S.filter even $ S.toList . S.filter odd $ S.copy $ each [1..10::Int]
[2,4,6,8,10] :> ([1,3,5,7,9] :> ())


    But 'separate' and 'unseparate' are functor-general.

-}

separate :: forall m f g r. (LMonad m, LFunctor f, LFunctor g)
         => Stream (Sum f g) m r ⊸ Stream f (Stream g m) r
separate str = destroyExposed
  str
  split
  (effect . lift)
  return
  where split :: Sum f g _ ⊸ Stream f (Stream g m) r
        split (InL fss) = wrap fss
        split (InR gss) = effect $ yields gss
{-#INLINABLE separate #-}



unseparate :: (LMonad m, LFunctor f, LFunctor g)
           =>  Stream f (Stream g m) r -> Stream (Sum f g) m r
unseparate str = destroyExposed
  str
  (wrap . InL)
  (join . maps InR)
  return
{-#INLINABLE unseparate #-}


unzips :: (LMonad m, LFunctor f, LFunctor g)
       => Stream (Compose f g) m r ⊸ Stream f (Stream g m) r
unzips str = destroyExposed
  str
  (\(Compose fgstr) -> Step (fmap (Effect . yields) fgstr))
  (Effect . lift)
  return
{-#INLINABLE unzips #-}

{-| Group layers in an alternating stream into adjoining sub-streams
    of one type or another.
-}
--groups :: (Monad m, Functor f, Functor g)
--           => Stream (Sum f g) m r
--           -> Stream (Sum (Stream f m) (Stream g m)) m r
--groups = loop
--  where
--  loop str = do
--    e <- lift $ inspect str
--    case e of
--      Left r -> return r
--      Right ostr -> case ostr of
--        InR gstr -> wrap $ InR (fmap loop (cleanR (wrap (InR gstr))))
--        InL fstr -> wrap $ InL (fmap loop (cleanL (wrap (InL fstr))))
--
--  cleanL  :: (Monad m, Functor f, Functor g) =>
--       Stream (Sum f g) m r -> Stream f m (Stream (Sum f g) m r)
--  cleanL = loop where
--    loop s = do
--     e <- lift $ inspect s
--     case e of
--      Left r           -> return (return r)
--      Right (InL fstr) -> wrap (fmap loop fstr)
--      Right (InR gstr) -> return (wrap (InR gstr))
--
--  cleanR  :: (Monad m, Functor f, Functor g) =>
--       Stream (Sum f g) m r -> Stream g m (Stream (Sum f g) m r)
----  cleanR = fmap (maps switch) . cleanL . maps switch
--  cleanR = loop where
--    loop s = do
--     e <- lift $ inspect s
--     case e of
--      Left r           -> return (return r)
--      Right (InL fstr) -> return (wrap (InL fstr))
--      Right (InR gstr) -> wrap (fmap loop gstr)
--{-#INLINABLE groups #-}

-- groupInL :: (Monad m, Functor f, Functor g)
--                      => Stream (Sum f g) m r
--                      -> Stream (Sum (Stream f m) g) m r
-- groupInL = loop
--   where
--   loop str = do
--     e <- lift $ inspect str
--     case e of
--       Left r -> return r
--       Right ostr -> case ostr of
--         InR gstr -> wrap $ InR (fmap loop gstr)
--         InL fstr -> wrap $ InL (fmap loop (cleanL (wrap (InL fstr))))
--   cleanL  :: (Monad m, Functor f, Functor g) =>
--        Stream (Sum f g) m r -> Stream f m (Stream (Sum f g) m r)
--   cleanL = loop where
--     loop s = dos
--      e <- lift $ inspect s
--      case e of
--       Left r           -> return (return r)
--       Right (InL fstr) -> wrap (fmap loop fstr)
--       Right (InR gstr) -> return (wrap (InR gstr))

{- | 'never' interleaves the pure applicative action with the return of the monad forever.
     It is the 'empty' of the 'Alternative' instance, thus

> never <|> a = a
> a <|> never = a

     and so on. If w is a monoid then @never :: Stream (Of w) m r@ is
     the infinite sequence of 'mempty', and
     @str1 \<|\> str2@ appends the elements monoidally until one of streams ends.
     Thus we have, e.g.

>>> S.stdoutLn $ S.take 2 $ S.stdinLn <|> S.repeat " " <|> S.stdinLn  <|> S.repeat " " <|> S.stdinLn
1<Enter>
2<Enter>
3<Enter>
1 2 3
4<Enter>
5<Enter>
6<Enter>
4 5 6

    This is equivalent to

>>> S.stdoutLn $ S.take 2 $ foldr (<|>) never [S.stdinLn, S.repeat " ", S.stdinLn, S.repeat " ", S.stdinLn ]

     Where 'f' is a monad, @(\<|\>)@ sequences the conjoined streams stepwise. See the
     definition of @paste@ <https://gist.github.com/michaelt/6c6843e6dd8030e95d58 here>,
     where the separate steps are bytestreams corresponding to the lines of a file.

     Given, say,

> data Branch r = Branch r r deriving Functor  -- add obvious applicative instance

    then @never :: Stream Branch Identity r@  is the pure infinite binary tree with
    (inaccessible) @r@s in its leaves. Given two binary trees, @tree1 \<|\> tree2@
    intersects them, preserving the leaves that came first,
    so @tree1 \<|\> never = tree1@

    @Stream Identity m r@ is an action in @m@ that is indefinitely delayed. Such an
    action can be constructed with e.g. 'untilJust'.

> untilJust :: (Monad m, Applicative f) => m (Maybe r) -> Stream f m r

    Given two such items, @\<|\>@ instance races them.
    It is thus the iterative monad transformer specially defined in
    <https://hackage.haskell.org/package/free-4.12.1/docs/Control-Monad-Trans-Iter.html Control.Monad.Trans.Iter>

    So, for example, we might write

>>> let justFour str = if length str == 4 then Just str else Nothing
>>> let four = untilJust (liftM justFour getLine)
>>> run four
one<Enter>
two<Enter>
three<Enter>
four<Enter>
"four"


    The 'Alternative' instance in
    <https://hackage.haskell.org/package/free-4.12.1/docs/Control-Monad-Trans-Free.html Control.Monad.Trans.Free>
    is avowedly wrong, though no explanation is given for this.


-}
never :: (LMonad m, LApplicative f) => Stream f m r
never =  let loop = Effect $ return $ Step $ pure loop in loop
{-#INLINABLE never #-}


delays :: (LMonadIO m, LFunctor f, LApplicative f) => Double -> Stream f m r
delays seconds = loop where
  loop = Effect $ liftIO (threadDelay delay) >> return (Step (pure loop))
  delay = fromInteger (truncate (1000000 * seconds))
{-#INLINABLE delays #-}

-- {-| Permit streamed actions to proceed unless the clock has run out.
--
-- -}
-- period :: (MonadIO m, Functor f) => Double -> Stream f m r -> Stream f m (Stream f m r)
-- period seconds str = do
--     utc <- liftIO getCurrentTime
--     let loop s = do
--           utc' <- liftIO getCurrentTime
--           if diffUTCTime utc' utc > (cutoff / 1000000000)
--             then return s
--             else case s of
--               Return r -> Return (Return r)
--               Effect m -> Effect (liftM loop m)
--               Step f   -> Step (fmap loop f)
--     loop str
--   where
--   cutoff = fromInteger (truncate (1000000000 * seconds))
-- {-#INLINABLE period #-}
--
--
-- {-| Divide a succession of phases according to a specified time interval. If time runs out
--     while an action is proceeding, it is allowed to run to completion. The clock is only then
--     restarted.
-- -}
-- periods :: (MonadIO m, Functor f) => Double -> Stream f m r -> Stream (Stream f m) m r
-- periods seconds s = do
--   utc <- liftIO getCurrentTime
--   loop (addUTCTime cutoff utc) s
--
--   where
--   cutoff = fromInteger (truncate (1000000000 * seconds)) / 1000000000
--   loop final stream = do
--     utc <- liftIO getCurrentTime
--     if utc > final
--       then loop (addUTCTime cutoff utc) stream
--       else case stream of
--         Return r  -> Return r
--         Effect m  -> Effect $ liftM (loop final) m
--         Step fstr -> Step $ fmap (periods seconds) (cutoff_ final (Step fstr))
--
--         -- do
--         --   let sloop s = do
--         --         utc' <- liftIO getCurrentTime
--         --         if final < utc'
--         --           then return s
--         --           else case s of
--         --             Return r -> Return (Return r)
--         --             Effect m -> Effect (liftM sloop m)
--         --             Step f   -> Step (fmap sloop f)
--         --   Step (Step (fmap (fmap (periods seconds) . sloop) fstr))
--           -- str <- m
--           -- utc' <- liftIO getCurrentTime
--           -- if diffUTCTime utc' utc > (cutoff / 1000000000)
--           --   then return (loop utc' str)
--           --   else return (loop utc str)
--         -- Step fs   -> do
--         --   let check str = do
--         --         utc' <- liftIO getCurrentTime
--         --         loop utc' str
--         --
-- {-# INLINABLE periods #-}
--
-- cutoff_ final str = do
--     let loop s = do
--           utc' <- liftIO getCurrentTime
--           if utc' > final
--             then Return s
--             else case s of
--               Return r -> Return (Return r)
--               Effect m -> Effect (liftM loop m)
--               Step f   -> Step (fmap loop f)
--     loop str

{- | Repeat a

-}

untilJust :: (LMonad m, LApplicative f) => m (Maybe r) -> Stream f m r
untilJust act = loop where
  loop = Effect $ do
    m <- act
    case m of
      Nothing -> return $ Step $ pure loop
      Just a  -> return $ Return a

-- Same semantics as splitsAt when linear?
--cutoff :: (LMonad m, LFunctor f) => Int -> Stream f m r -> Stream f m (Maybe r)
--cutoff = loop where
--  loop 0 str = return Nothing
--  loop n str = do
--      e <- lift $ inspect str
--      case e of
--        Left r -> return (Just r)
--        Right (frest) -> Step $ fmap (loop (n-1)) frest
