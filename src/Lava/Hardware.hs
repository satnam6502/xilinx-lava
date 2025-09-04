{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
module Lava.Hardware
where
import Data.Array.Ranked

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
  lut2 :: (Bool -> Bool -> Bool) -> (bit, bit) -> m bit
  carry4 :: bit -> bit -> Array 4 bit -> Array 4 bit -> m (Array 4 bit, Array 4 bit) -- ci -> cyinit -> (di, s) -> (o, co)

xor2 :: Hardware m bit => (bit, bit) -> m bit
xor2 = lut2 (/=)

