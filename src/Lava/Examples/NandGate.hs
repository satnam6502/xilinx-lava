module Lava.Examples.NandGate (nandGate)
where

import Lava

nandGate :: Hardware m bit => (bit, bit) -> m bit
nandGate = and2 >=> inv

