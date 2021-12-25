{-# LANGUAGE TypeFamilies #-}

module Positive.Data.HKD where


-- HKD

data New


data FromDatabase


type family P t f a where
  P FromDatabase f a = a
  P New f a = f a
