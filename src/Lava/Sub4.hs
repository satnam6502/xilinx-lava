{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lava.Sub4 where

import Lava
import Data.Array.Shaped

-- If a >= b carryOut = 1, else carryOut = 0.
-- sub4 computes a - b for two unsigned values a and b, with wrap-around arithmetic.
sub4 :: Hardware m bit => Array '[4] bit -> Array '[4] bit -> m (Array '[4] bit, bit)
sub4 a b
  = do ps0 :: bit <- xnor2 (aL!!0, bL!!0)
       ps1 :: bit <- xnor2 (aL!!1, bL!!1)
       ps2 :: bit <- xnor2 (aL!!2, bL!!2)
       ps3 :: bit <- xnor2 (aL!!3, bL!!3)
       b0 <- zero
       b1 <- one
       (sumOut, carryOut) <- carry4 b0 b1 a (fromList [ps0, ps1, ps2, ps3])
       return (sumOut, unScalar (carryOut `index` 3))
    where
    aL = toList a
    bL = toList b

sub4Top :: RTL ()
sub4Top
  = do setModuleName "sub4"
       a :: Array '[4] (Net Bit)  <- inputVec "a" BitType
       b :: Array '[4] (Net Bit)  <- inputVec "b" BitType
       (subOut, carryOut) <- sub4 a b
       outputVec "subOut" subOut BitType
       output "carryOut" carryOut
