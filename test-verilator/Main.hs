module Main (main) where

import Lava
import Lava.Examples.NandGate

main :: IO ()
main
  = do writeSystemVerilog nandGateTop
       writeSystemVerilogSimulation nandGateTop [[L, L], [H, L], [L, H], [H, H]]
