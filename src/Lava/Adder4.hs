{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lava.Adder4 where

import Lava
import Data.Array.Shaped

adder4 :: Hardware m bit => Array '[4] bit -> Array '[4] bit -> m (Array '[4] bit)
adder4 a b
  = do ps0 :: bit <- xor2 (aL!!0, bL!!0)
       ps1 :: bit <- xor2 (aL!!1, bL!!1)
       ps2 :: bit <- xor2 (aL!!2, bL!!2)
       ps3 :: bit <- xor2 (aL!!3, bL!!3)
       b0 <- zero
       (sumOut, _) <- carry4 b0 b0 a (fromList [ps0, ps1, ps2, ps3])
       return sumOut
    where
    aL = toList a
    bL = toList b

adder4Top :: RTL ()
adder4Top
  = do setModuleName "adder4"
       a <- inputVec "a" 4 BitType
       b <- inputVec "b" 4 BitType
       sumOut <- adder4 a b
       outputVec "sumOut" sumOut BitType
