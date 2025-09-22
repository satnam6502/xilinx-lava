module Main (main) where

import Lava
import System.Process
import Lava.Examples.NandGate
import Lava.Examples.InvN
import Lava.OneBitAdder
import Lava.Adder4
import Lava.Sub4
import Lava.Mux
import Lava.TwoSorter
import Lava.Sorter

main :: IO ()
main
  = do writeSystemVerilog altNandGateTop
       writeSystemVerilogSimulation altNandGateTop [[L, L], [H, L], [L, H], [H, H]]
       callProcess "verilator" ["+1800-2017ext+sv", "verilator.vlt", "--binary",  "--trace",  "-Wall", "--top-module",  "nandgate_sim",  "altNandGate.sv", "nandgate_sim.sv"]
       callProcess "obj_dir/Vnandgate_sim" ["+trace"]
       writeSystemVerilog invNTop
       writeSystemVerilog inv4x2Top
       writeSystemVerilog oneBitAdderTop
       writeSystemVerilog adder4Top
       writeSystemVerilog sub4Top
       writeSystemVerilog muxN8Top
       writeSystemVerilog twoSorterTop
       writeSystemVerilog twoSorterRegTop
       writeSystemVerilog max2Top
       writeSystemVerilog sorterComb4Top
       writeSystemVerilog sorter4Top
       writeSystemVerilog sorter8Top
       writeSystemVerilog sorter16Top
       writeSystemVerilog sorter32Top
       writeSystemVerilog sorter64Top
       writeSystemVerilog nand2LayoutTop
