module Main (main) where

import Lava
import System.Process
import Lava.Examples.NandGate

main :: IO ()
main
  = do writeSystemVerilog nandGateTop
       writeSystemVerilogSimulation nandGateTop [[L, L], [H, L], [L, H], [H, H]]
       callProcess "verilator" ["+1800-2017ext+sv", "verilator.vlt", "--binary",  "--trace",  "-Wall", "--top-module",  "nandgate_sim",  "nandgate.sv", "nandgate_sim.sv"]
       callProcess "obj_dir/Vnandgate_sim" ["+trace"]
