{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Lava.Sorter where
import GHC.TypeNats
import Data.Array.Shaped

import Lava.Hardware
import Lava.Combinators
import Lava.TwoSorter
import Lava.RTL hiding (One)

-- Define an inductive representation of natural numbers that
-- can be used to describe the degree of the butterfly.
-- NOTE: these numbers start at 1.
data N = One | Succ N

-- A type family to represent 2^ of a N-type value as a 2^ of a Nat-type value.
type family ToExpNat (n :: N) :: Nat where
            ToExpNat One = 2
            ToExpNat (Succ n) = 2 * ToExpNat n


-- A butterfly of degree n maps 2^n inputs to 2^n outputs.
class Bfly (n :: N) where
  bfly :: forall m bit . Hardware m bit => (Array '[2] (Array '[4] bit) -> m (Array '[2] (Array '[4] bit))) ->
                                           Array '[ToExpNat n] (Array '[4] bit) -> m (Array '[ToExpNat n] (Array '[4] bit) )

-- A butterfly of degree 1 is just the r component i.e. it maps 2 inputs to 2 outputs.
instance Bfly One where
  bfly r a = r a

-- A butterfly of degree n is recursively defined in terms of two smaller butterflies of degree (n-1).
instance (KnownNat (ToExpNat n), Bfly n) => Bfly (Succ n) where
  bfly r = ilv (bfly @n r) >-> evens r

class BatchersSorter (n :: N) where
  batchersSorter :: Hardware m bit => (Array '[2] (Array '[4] bit) -> m (Array '[2] (Array '[4] bit))) ->
                                      Array '[ToExpNat n] (Array '[4] bit) -> m (Array '[ToExpNat n] (Array '[4] bit) )

instance BatchersSorter One where
  batchersSorter :: Hardware m bit => (Array '[2] (Array '[4] bit) -> m (Array '[2] (Array '[4] bit))) ->
                                       Array '[ToExpNat 'One] (Array '[4] bit) -> m (Array '[ToExpNat 'One] (Array '[4] bit))
  batchersSorter sorter2 = sorter2

instance (KnownNat (ToExpNat n), Bfly n, BatchersSorter n) => BatchersSorter (Succ n) where
  batchersSorter :: (KnownNat (ToExpNat n), Bfly n, BatchersSorter n, Hardware m bit) =>
                    (Array '[2] (Array '[4] bit) -> m (Array '[2] (Array '[4] bit))) ->
                    Array '[ToExpNat ('Succ n)] (Array '[4] bit) -> m (Array '[ToExpNat ('Succ n)] (Array '[4] bit))
  batchersSorter sorter2 = two (batchersSorter @n sorter2) >-> sndRev >-> bfly @(Succ n) sorter2

sndRev :: forall a n m. (KnownNat n, Monad m) => Array '[2 * n] a -> m (Array '[2 * n] a)
sndRev a
  = do ah :: Array '[2] (Array '[n] a) <- halve a
       let a1 :: Array '[n] a
           a1 = index (ravel ah) 1
       unhalve $ fromList @'[2] [index (ravel ah) 0, rev @'[0] a1]


combinationalSorter :: forall n m bit . (KnownNat (ToExpNat n), Hardware m bit, BatchersSorter n) =>
                   Array '[ToExpNat n] (Array '[4] bit) -> m (Array '[ToExpNat n] (Array '[4] bit) )
combinationalSorter = batchersSorter @n twoSorterCombinatonal


sorterComb4Top :: RTL ()
sorterComb4Top
  = do setModuleName "sorterComb4"
       a :: Array '[4] (Net (VectorKind '[4] bit)) <- inputVec "a" (VecType [4] BitType)
       let a' = mapA smash a
       b <- combinationalSorter @(Succ One) a'
       let b' :: Array '[4] (Net (VectorKind '[4] bit)) = unsmashA b
       outputVec "b" b' (VecType [4] BitType)

pipelinedSorter :: forall n m bit . (KnownNat (ToExpNat n), Hardware m bit, BatchersSorter n) =>
                   Array '[ToExpNat n] (Array '[4] bit) -> m (Array '[ToExpNat n] (Array '[4] bit) )
pipelinedSorter = batchersSorter @n twoSorterA

sorter4 :: Hardware m bit => Array '[4] (Array '[4] bit) -> m (Array '[4] (Array '[4] bit))
sorter4 = pipelinedSorter @(Succ One)

sorter4Top :: RTL ()
sorter4Top
  = do setModuleName "sorter4"
       setClockNet "clk"
       setActiveLowResetNet "rstN"
       a :: Array '[4] (Net (VectorKind '[4] bit)) <- inputVec "a" (VecType [4] BitType)
       let a' = mapA smash a
       b <- sorter4 a'
       let b' :: Array '[4] (Net (VectorKind '[4] bit)) = unsmashA b
       outputVec "b" b' (VecType [4] BitType)

sorter8 :: Hardware m bit => Array '[8] (Array '[4] bit) -> m (Array '[8] (Array '[4] bit))
sorter8 = pipelinedSorter @(Succ (Succ One))

sorter8Top :: RTL ()
sorter8Top
  = do setModuleName "sorter8"
       setClockNet "clk"
       setActiveLowResetNet "rstN"
       a :: Array '[8] (Net (VectorKind '[4] bit)) <- inputVec "a" (VecType [4] BitType)
       let a' = mapA smash a
       b <- sorter8 a'
       let b' :: Array '[8] (Net (VectorKind '[4] bit)) = unsmashA b
       outputVec "b" b' (VecType [4] BitType)

sorter16 :: Hardware m bit => Array '[16] (Array '[4] bit) -> m (Array '[16] (Array '[4] bit))
sorter16 = pipelinedSorter @(Succ (Succ (Succ One)))

sorter16Top :: RTL ()
sorter16Top
  = do setModuleName "sorter16"
       setClockNet "clk"
       setActiveLowResetNet "rstN"
       a :: Array '[16] (Net (VectorKind '[4] bit)) <- inputVec "a" (VecType [4] BitType)
       let a' = mapA smash a
       b <- sorter16 a'
       let b' :: Array '[16] (Net (VectorKind '[4] bit)) = unsmashA b
       outputVec "b" b' (VecType [4] BitType)

sorter32 :: Hardware m bit => Array '[32] (Array '[4] bit) -> m (Array '[32] (Array '[4] bit))
sorter32 = pipelinedSorter @(Succ (Succ (Succ (Succ One))))

sorter32Top :: RTL ()
sorter32Top
  = do setModuleName "sorter32"
       setClockNet "clk"
       setActiveLowResetNet "rstN"
       a :: Array '[32] (Net (VectorKind '[4] bit)) <- inputVec "a" (VecType [4] BitType)
       let a' = mapA smash a
       b <- sorter32 a'
       let b' :: Array '[32] (Net (VectorKind '[4] bit)) = unsmashA b
       outputVec "b" b' (VecType [4] BitType)

sorter64 :: Hardware m bit => Array '[64] (Array '[4] bit) -> m (Array '[64] (Array '[4] bit))
sorter64 = pipelinedSorter @(Succ (Succ (Succ (Succ (Succ One)))))

sorter64Top :: RTL ()
sorter64Top
  = do setModuleName "sorter64"
       setClockNet "clk"
       setActiveLowResetNet "rstN"
       a :: Array '[4] (Net Bit)  <- inputVec "a" BitType
       b <- sorter64 (fromList (replicate 64 a))
       outputVec "b" (unScalar (b `index` 0)) BitType
