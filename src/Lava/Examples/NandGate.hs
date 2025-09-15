{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lava.Examples.NandGate
where
import Lava

altNandGate :: Hardware m bit => [bit] -> m bit
altNandGate = andGate >=> invGate

altNandGateTop :: RTL ()
altNandGateTop
  = do setModuleName "altNandGate"
       a <- input "a" BitType
       b <- input "b" BitType
       c <- altNandGate [a, b] 
       output "c" c

-- A NAND gate with layout combinators.

nandLayout :: Hardware m bit => (bit, bit) -> m bit
nandLayout = and2 >-> inv

nand2LayoutTop :: RTL ()
nand2LayoutTop
  = do setModuleName "nand2Layout"
       a <- input "a" BitType
       b <- input "b" BitType
       c <- nandLayout (a, b) 
       output "c" c
