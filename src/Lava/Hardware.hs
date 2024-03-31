{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
module Lava.Hardware
where
import Lava.Combinators

class Monad m => Hardware m bit where
  inv :: bit -> m bit
  and2 :: (bit, bit) -> m bit

nand :: Hardware m bit => (bit, bit) -> m bit
nand = and2 >=> inv

