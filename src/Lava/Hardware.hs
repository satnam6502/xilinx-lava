{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lava.Hardware
where

class Monad m => Hardware m bit | m -> bit where
  -- Generic gates
  inv :: bit -> m bit
  and2 :: (bit, bit) -> m bit
  or2 :: (bit, bit) -> m bit
  xor2 :: (bit, bit) -> m bit
  nor2 :: (bit, bit) -> m bit
  xnor2 :: (bit, bit) -> m bit
  -- Generic unit delay
  delay :: bit -> m bit
  -- Xilinx UNISIM gates
  xorcy :: (bit, bit) -> m bit -- (cin, li) -> partial sum
  muxcy :: (bit, (bit, bit)) -> m bit -- (s, (ci, di)) -> sum
