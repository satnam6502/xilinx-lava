{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lava.Hardware
where

class Monad m => Hardware m bit | m -> bit where
  inv :: bit -> m bit
  and2 :: (bit, bit) -> m bit
  or2 :: (bit, bit) -> m bit
  xor2 :: (bit, bit) -> m bit
  nor2 :: (bit, bit) -> m bit
  xnor2 :: (bit, bit) -> m bit
  delay :: bit -> m bit

