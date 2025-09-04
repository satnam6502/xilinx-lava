module Main (main) where

import Lava
import System.Process
import Lava.Examples.NandGate
import Lava.OneBitAdder

main :: IO ()
main
  = do writeSystemVerilog altNandGateTop
       writeSystemVerilogSimulation altNandGateTop [[L, L], [H, L], [L, H], [H, H]]
       callProcess "verilator" ["+1800-2017ext+sv", "verilator.vlt", "--binary",  "--trace",  "-Wall", "--top-module",  "nandgate_sim",  "altNandGate.sv", "nandgate_sim.sv"]
       callProcess "obj_dir/Vnandgate_sim" ["+trace"]

       writeSystemVerilog oneBitAdderTop
