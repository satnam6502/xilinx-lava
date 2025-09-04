{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lava.Examples.NandGate (altNandGate, altNandGateTop)
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

