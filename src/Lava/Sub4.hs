{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lava.Sub4 where

import Lava
import Data.Array.Shaped
import GHC.Stack

-- If a >= b carryOut = 1, else carryOut = 0.
-- sub4 computes a - b for two unsigned values a and b, with wrap-around arithmetic.
sub4 :: Hardware m bit => (Array '[4] bit, Array '[4] bit) -> m (Array '[4] bit, bit)
sub4 (a, b)
  =  (zipArray >|> vmap xnor2 >|> pairLeft a >|> carry4L) (a, b)

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
sub4OnlyCarryOut (a, b) = (zipArray >|> vmap xnor2 >|> pairLeft a >|> carry4OnlyCarryOut) (a, b)

