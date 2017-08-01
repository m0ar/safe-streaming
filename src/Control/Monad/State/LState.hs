{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.State.LState where

import Data.Linear (($), (.))
import Control.Monad.LMonad
import Control.Applicative.LApplicative
import Data.Functor.LFunctor

-- | A linear state function on some state @s@.
newtype LState s a = LState { runLState :: s ⊸ (s, a) }

-- | This looks exactly like @runLState@, but record projections are not in
-- general linear. Consider @data P a b = P { p1 :: a, p2 :: b }@, where @p1
-- :: P a b ⊸ a@ which of course is illegal.
--
-- There is only support for fully linear records, so a projection in general
-- will be considered unrestricted. One workaround is to define your explicity
-- linear projections liks this.
unrunLState :: LState s a ⊸ (s ⊸ (s, a))
unrunLState (LState cont) = cont


instance LFunctor (LState s) where
  fmap :: forall a b. (a ⊸ b) ⊸ LState s a ⊸ LState s b
  fmap f (LState cont) = LState $ fmap f . cont
  -- LFunctor instance for tuples maps over second component


instance LApplicative (LState s) where
  pure :: a ⊸ LState s a
  pure x = LState $ \s -> (s, x)

  (<*>) :: forall a b. LState s (a ⊸ b) ⊸ LState s a ⊸ LState s b
  (LState f) <*> (LState cont) = LState $ apply . f
    where
      apply :: (s, a ⊸ b) ⊸ (s, b)
      apply (s', f') = f' <$> cont s'


instance LMonad (LState s) where
  (>>=) :: forall a b. LState s a ⊸ (a ⊸ LState s b) ⊸ LState s b
  LState cont >>= f = LState $ \s -> apply f (cont s)
    where
      apply :: (a ⊸ LState s b) ⊸ (s, a) ⊸ (s, b)
      apply f' (s', a) = (unrunLState $ f' a) s'


----------------------------------
-- Different kinds of linear state
----------------------------------

-- | A fully linear type of state.
--
-- Cannot be get (because @s ⊸ (s,s)@ will not pass the linearity check), but
-- it can be modified freely.
data LS a where LS :: a ⊸ LS a


-- | An unrestricted type of state.
--
-- Using this with LState, the state itself is threaded linearly, but the @a@ is
-- unrestricted and can be used freely.
--
-- This makes it possible to have get return the state, since @s ⊸ (s,s)@ is
-- otherwise unlawful.
data S a where S :: a -> S a


-- | A partially linear type of state.
--
-- The same applies as above, but the @a@ has to
-- be treated linearly (and hence cannot be @get@), but the @b@ can be used
-- freely.
data PLS a b where PLS :: a ⊸ b -> PLS a b


-- | Modify the state with some function.
--
-- This can be done regardless of linearity in the state, because it threads
-- it linearly and returns nothing.
modify :: (s ⊸ s) ⊸ LState s ()
modify fn = LState $ \s -> (fn s, ())


-- | Type class for defining what @get@:ing your state type means, since this
-- depends on what it contains that is unrestricted.
class Gettable s where
  type GetRes s :: *
  get :: LState s (GetRes s)


-- | From a fully linear state, @get@ can only return @()@.
instance Gettable (LS a) where
  type GetRes (LS a) = ()
  get = LState $ \s -> (s, ())


-- | From an unrestricted state, @get@ can return the whole state.
instance Gettable (S a) where
  type GetRes (S a) = a
  get = LState $ \(S a) -> (S a, a)


-- | From a partially linear state, @get@ can return the unrestricted
-- components.
instance Gettable (PLS a b) where
  type GetRes (PLS a b) = b
  get = LState $ \(PLS a b) -> (PLS a b, b)
