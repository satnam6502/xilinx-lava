{-# LANGUAGE DataKinds #-}
module Lava.Examples.NandGate (nandGate, nandGateSV)
where
import Lava

nandGate :: Hardware m bit => (bit, bit) -> m bit
nandGate = and2 >=> inv

nandGateTop :: RTL ()
nandGateTop
  = do setModuleName "nandgate"
       a <- input "a" BitType
       b <- input "b" BitType
       c <- nandGate (a, b) :: RTL Bit
       output "c" c

nandGateSV :: IO ()
nandGateSV = writeSystemVerilog "nandgate" nandGateTop

