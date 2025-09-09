{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Lava.Sorter where
import GHC.TypeNats
import Data.Array.Shaped

import Lava.Combinators

-- Define an inductive representation of natural numbers that
-- can be used to describe the degree of the butterfly.
data N = Z | S N
data INat (n :: N) where
  Zero :: INat Z
  Succ :: INat n -> INat (S n)

-- A type family to represent 2^ of a N-type value as a 2^ of a Nat-type value.
type family ToExpNat (n :: N) :: Nat where
            ToExpNat Z = 1
            ToExpNat (S n) = 2 * ToExpNat n

data KNat n where
  KNat :: KnownNat n => KNat n

mulKNat :: KNat a -> KNat b -> KNat (a * b)
mulKNat KNat KNat = KNat

inatToExpKNat :: INat n -> KNat (ToExpNat n)
inatToExpKNat Zero = KNat
inatToExpKNat (Succ n) = mulKNat KNat (inatToExpKNat n)

-- A butterfly of degree n maps 2^n inputs to 2^n outputs.
class Bfly (n :: N) where
  bfly :: INat n -> (Array '[2] a -> Array '[2] a) -> Array '[ToExpNat n] a -> Array '[ToExpNat n] a

-- A butterfly of degree 1 is just the r component i.e. it maps 2 inputs to 2 outputs.
instance Bfly (S Z) where
  bfly (Succ Zero) r a = r a

-- A butterfly of degree n is recursively defined in terms of two smaller butterflies of degree (n-1).
instance {-# OVERLAPS #-} (Bfly n) => Bfly (S n) where
  bfly (Succ n) r = case inatToExpKNat n of
    KNat -> evens r . ilv (bfly n r)

