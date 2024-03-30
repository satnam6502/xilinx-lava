{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
module Lava.Hardware
where

class Monad m => Hardware m bit where
  inv :: bit -> m bit
  and2 :: (bit, bit) -> m bit

