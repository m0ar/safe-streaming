{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.State.LState where

-- | A linear state function for some state @s@.
type LState s a = s ⊸ (s, a)


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
modify fn s = (fn s, ())


-- | Type class for defining what @get@:ing your state type means,
-- since this depends on what it contains that is unrestricted.
class Gettable s where
  type GetRes s :: *
  get :: LState s (GetRes s)

-- | From a fully linear state, @get@ can only return @()@.
instance Gettable (LS a) where
  type GetRes (LS a) = ()
  get s = (s, ())

-- | From an unrestricted state, @get@ can return the whole state.
instance Gettable (S a) where
  type GetRes (S a) = a
  get (S a) = (S a, a)

-- | From a partially linear state, @get@ can return the unrestricted
-- components.
instance Gettable (PLS a b) where
  type GetRes (PLS a b) = b
  get (PLS a b) = (PLS a b, b)
