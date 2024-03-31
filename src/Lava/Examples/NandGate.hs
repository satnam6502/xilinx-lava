{-# LANGUAGE DataKinds #-}
module Lava.Examples.NandGate (nandGate, nandGateTop)
where

import Lava

nandGate :: Hardware m bit => (bit, bit) -> m bit
nandGate = and2 >=> inv

nandGateRTL :: (Bit, Bit) -> RTL Bit
nandGateRTL = and2 >=> inv

nandGateTop :: RTL ()
nandGateTop
  = do a <- input "a" BitType
       b <- input "b" BitType
       c <- nandGateRTL (a, b)
       output "c" c

