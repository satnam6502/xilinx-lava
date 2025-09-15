{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lava.Hardware
where
import Data.Array.Shaped

infixr 5 >->

class Monad m => Hardware m bit | m -> bit where
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
  lut1 :: (Bool -> Bool) -> bit -> m bit
  lut2 :: (Bool -> Bool -> Bool) -> (bit, bit) -> m bit
  lut3 :: (Bool -> Bool -> Bool -> Bool) -> (bit, bit, bit) -> m bit
  carry4 :: bit -> bit -> Array '[4] bit -> Array '[4] bit -> m (Array '[4] bit, Array '[4] bit) -- ci -> cyinit -> (di, s) -> (o, co)
  reg :: bit -> m bit
  -- Layout combinators
  (>->) :: (a -> m b) -> (b -> m c) -> a -> m c -- Left to right serial composition

inv :: Hardware m bit => bit -> m bit
inv = lut1 not

and2 :: Hardware m bit => (bit, bit) -> m bit
and2 = lut2 (&&)

xor2 :: Hardware m bit => (bit, bit) -> m bit
xor2 = lut2 (/=)

-- a XOR (not b)
xor2b2 :: Hardware m bit => (bit, bit) -> m bit
xor2b2 = lut2 (\a b -> a /= not b)

xnor2 :: Hardware m bit => (bit, bit) -> m bit
xnor2 = lut2 (==)
