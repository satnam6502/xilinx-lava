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

module Lava.Sorter where
import GHC.TypeNats

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