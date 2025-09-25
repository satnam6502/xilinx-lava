{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Lava.Sub4 where

import Lava
import Data.Array.Shaped
import GHC.Stack
import GHC.TypeLits

-- If a >= b carryOut = 1, else carryOut = 0.
-- sub4 computes a - b for two unsigned values a and b, with wrap-around arithmetic.
sub4 :: Hardware m bit => (Array '[4] bit, Array '[4] bit) -> m (Array '[4] bit, bit)
sub4 (a, b)
  =  (zipArray >=> vmap xnor2 >|> pairLeft a >=> carry4L) (a, b)

carry4L :: Hardware m bit => (Array '[4] bit, Array '[4] bit) -> m (Array '[4] bit, bit)
carry4L (a, b)
  = do b0 <- zero
       b1 <- one
       (diff, cout) <- carry4 b0 b1 b a
       return (diff, unScalar (cout `index` 3))

sub4Top :: RTL ()
sub4Top
  = do setModuleName "sub4"
       a :: Array '[4] (Net Bit)  <- inputVec "a" BitType
       b :: Array '[4] (Net Bit)  <- inputVec "b" BitType
       (subOut, carryOut) <- sub4 (a, b)
       outputVec "subOut" subOut BitType
       output "carryOut" carryOut

carry4OnlyCarryOut :: Hardware m bit => (Array '[4] bit, Array '[4] bit) -> m bit
carry4OnlyCarryOut (a, b)
  = do (_, carryOut) <- carry4L (a, b)
       return carryOut

sub4OnlyCarryOut :: (HasCallStack, Hardware m bit) => (Array '[4] bit, Array '[4] bit) -> m bit
sub4OnlyCarryOut (a, b) = (zipArray >=> vmap xnor2 >|> pairLeft a >=> carry4OnlyCarryOut) (a, b)

sub4A :: Hardware m bit => Array '[2] (Array '[4] bit) -> m (Array '[4] bit)
sub4A v = do (s, _) <- sub4 (a, b)
             return s
          where
          a = unScalar (v `index` 0)
          b = unScalar (v `index` 1)

pairArray :: forall a n . KnownNat n => Array '[n * 2] a  -> Array '[n, 2] a
pairArray = reshape @'[n, 2]

pairArrayUnravel :: forall a n . KnownNat n=> Array '[n * 2] a -> Array '[n] (Array '[2] a)
pairArrayUnravel = unravel . pairArray

sub128 :: Hardware m bit => Array '[128] (Array '[4] bit) -> m (Array '[64] (Array '[4] bit))
sub128 a = vmap sub4A (pairArrayUnravel a)


sub128Top :: RTL ()
sub128Top
  = do setModuleName "sub128"
       a :: Array '[4] (Net Bit) <- inputVec "a" BitType
       b <- sub128 (fromList (replicate 128 a))
       outputVec "subout" (unScalar (b `index` 0)) BitType
