{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Data.HList
  ( HList(..)
  , All
  , hLength
  , hHead
  ) where

import Data.Kind (Constraint, Type)

data HList (xs :: [Type]) where
  HNil :: HList '[]
  (:#) :: x -> HList xs -> HList (x ': xs)

infixr 5 :#

hLength :: HList xs -> Int
hLength HNil = 0
hLength (_ :# t) = succ $ hLength t

hHead :: HList (a ': as) -> a
hHead (h :# _) = h

type family All (c :: k -> Constraint) (xs :: [Type]) :: Constraint where
  All c '[] = ()
  All c (x ': xs) = (c x, All c xs)

instance All Eq xs => Eq (HList xs) where
  HNil == HNil = True
  (x :# xs) == (y :# ys) = x == y && xs == ys

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (x :# xs) (y :# ys) =
    compare x y <> compare xs ys

instance All Show xs => Show (HList xs) where
  show HNil = "HNil"
  show (x :# xs) = show x <> " :# " <> show xs
