{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
module Lava.Hardware
where

class Hardware m bit where
  inv :: bit -> m bit
  and2 :: (bit, bit) -> m bit

