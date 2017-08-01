{-#LANGUAGE CPP, DeriveDataTypeable, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE GADTs #-}
module Data.Functor.LOf where

import Data.Functor.LFunctor
import Control.Applicative ()
import Data.Monoid ()
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor
#endif
import Data.Data
import Data.Typeable ()

-- | A left-strict pair; the base functor for streams of individual elements.
-- Unrestricted in its first argument, this allows us to write intuitive semantics
-- for drop, filter and so on in the streaming prelude while still being an LFunctor.
-- This is neat because it allows us to keep the monadic actions linear, but not the
-- resulting values (only the first one can cause critical problems with streaming).
data LOf a b where (:>) :: !a -> b ⊸ LOf a b
    deriving (Data, Eq, Foldable, Ord,
              Read, Show, Traversable, Typeable)
infixr 5 :>

instance (Monoid a, Monoid b) => Monoid (LOf a b) where
  mempty = mempty :> mempty
  {-#INLINE mempty #-}
  mappend (m :> w) (m' :> w') = mappend m m' :> mappend w w'
  {-#INLINE mappend #-}

instance Functor (LOf a) where
  fmap f (a :> x) = a :> f x
  {-#INLINE fmap #-}
  a <$ (b :> _)   = b :> a
  {-#INLINE (<$) #-}

instance LFunctor (LOf a) where
  fmap f (a :> x) = a :> f x
  {-# INLINE fmap #-}

#if MIN_VERSION_base(4,8,0)
instance Bifunctor LOf where
  bimap f g (a :> b) = f a :> g b
  {-#INLINE bimap #-}
  first f   (a :> b) = f a :> b
  {-#INLINE first #-}
  second g  (a :> b) = a :> g b
  {-#INLINE second #-}
#endif

instance Monoid a => Applicative (LOf a) where
  pure x = mempty :> x
  {-#INLINE pure #-}
  m :> f <*> m' :> x = mappend m m' :> f x
  {-#INLINE (<*>) #-}
  m :> _ *> m' :> y  = mappend m m' :> y
  {-#INLINE (*>) #-}
  m :> x <* m' :> _  = mappend m m' :> x
  {-#INLINE (<*) #-}

instance Monoid a => Monad (LOf a) where
  return x = mempty :> x
  {-#INLINE return #-}
  m :> _ >> m' :> y = mappend m m' :> y
  {-#INLINE (>>) #-}
  m :> x >>= f = let m' :> y = f x in mappend m m' :> y
  {-#INLINE (>>=) #-}
