{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lava.Examples.NandGate (nandGate, nandGateTop)
where
import Lava

nandGate :: Hardware m bit => (bit, bit) -> m bit
nandGate = and2 >=> inv

nandGateTop :: RTL ()
nandGateTop
  = do setModuleName "nandgate"
       a <- input "a" BitType
       b <- input "b" BitType
       c <- nandGate (a, b) 
       output "c" c

