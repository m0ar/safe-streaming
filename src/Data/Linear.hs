{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Linear where

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

