{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lava.Hardware
where
import Data.Array.Shaped

class Monad m => Hardware m bit where
  -- Generic gates
  zero :: m bit
  one :: m bit
  invGate :: bit -> m bit
  andGate :: [bit] -> m bit
  orGate :: [bit] -> m bit
  xorGate :: [bit] -> m bit
  norGate :: [bit] -> m bit
  xnorGate :: [bit] -> m bit
  -- Generic unit delay
  delay :: bit -> m bit
  -- Xilinx UNISIM gates
  xorcy :: (bit, bit) -> m bit -- (cin, li) -> partial sum
  muxcy :: (bit, (bit, bit)) -> m bit -- (s, (ci, di)) -> sum
  lut2 :: (Bool -> Bool -> Bool) -> (bit, bit) -> m bit
  lut3 :: (Bool -> Bool -> Bool -> Bool) -> (bit, bit, bit) -> m bit
  carry4 :: bit -> bit -> Array '[4] bit -> Array '[4] bit -> m (Array '[4] bit, Array '[4] bit) -- ci -> cyinit -> (di, s) -> (o, co)
  reg :: bit -> m bit

xor2 :: Hardware m bit => (bit, bit) -> m bit
xor2 = lut2 (/=)

-- a XOR (not b)
xor2b2 :: Hardware m bit => (bit, bit) -> m bit
xor2b2 = lut2 (\a b -> a /= (not b))

xnor2 :: Hardware m bit => (bit, bit) -> m bit
xnor2 = lut2 (==)
