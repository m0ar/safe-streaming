{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GADTs#-}

module Data.Linear where

import Prelude (Show(..), Eq(..))

id :: a ⊸ a
id a = a

liftUnit :: a ⊸ () ⊸ a
liftUnit a () = a 

flip :: (a ⊸ b ⊸ c ) ⊸ b ⊸ a ⊸ c
flip f b a = f a b

infixr 0 $

($) :: (a ⊸ b) ⊸ a ⊸ b
f $ x = f x
{-# INLINE ($) #-}

(.) :: (b ⊸ c) ⊸ (a ⊸ b) ⊸ a ⊸ c
(.) f g a = f $ g a

data Unrestricted a where
  Unrestricted :: a -> Unrestricted a
  deriving (Show, Eq)

getUnrestricted :: Unrestricted a ⊸ a
getUnrestricted (Unrestricted a) = a


