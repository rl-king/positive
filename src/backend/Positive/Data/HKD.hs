{-# LANGUAGE TypeFamilies #-}

module Positive.Data.HKD where

-- HKD

data New

data FromDatabase

type family Unwrap t f a where
  Unwrap FromDatabase f a = a
  Unwrap New f a = f a
