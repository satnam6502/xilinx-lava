module Lava.OneBitAdder where

import Lava

oneBitAdder :: Hardware m bit => (bit, (bit, bit)) -> m (bit, bit)
oneBitAdder (cin, (a, b))
  = do part_sum <- xor2 (a, b)
       sumBit <- xorcy (cin, part_sum)
       cout <- muxcy (part_sum, (a, cin))
       return (sumBit, cout)

oneBitAdderTop :: RTL ()
oneBitAdderTop
  = do setModuleName "oneBitAdder"
       cin <- input "cin" BitType
       a <- input "a" BitType
       b <- input "b" BitType
       (sumBit, cout) <- oneBitAdder (cin, (a, b) )
       output "sum" sumBit
       output "cout" cout
