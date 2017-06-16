{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Linear where

const :: a ⊸ b -> a
const a _ = a
