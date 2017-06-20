{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Linear where

liftUnit :: a ⊸ () ⊸ a
liftUnit a () = a 

flip :: (a ⊸ b ⊸ c ) ⊸ b ⊸ a ⊸ c
flip f b a = f a b

