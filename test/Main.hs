module Main (main) where

import Lava
import Lava.Examples.NandGate
import Data.List (transpose)

invGateTest :: Bool
invGateTest = runSim (inv [False, True]) == map not [False, True]

pair2tuple :: [a] -> (a, a)
pair2tuple [x, y] = (x, y)
pair2tuple _ = error "pair2tuple"

bool2Inputs :: ([Bool], [Bool])
bool2Inputs = pair2tuple $ transpose [[x, y] | x <- [False, True], y <- [False, True]]

nandGateGolden :: ([Bool], [Bool]) -> [Bool]
nandGateGolden (xs, ys) = [not (x && y) | (x, y) <- zip xs ys]

nandGateTest :: Bool
nandGateTest = runSim (nandGate bool2Inputs) ==
               nandGateGolden bool2Inputs

check :: String -> Bool -> IO ()
check testName passed
  = if passed then
      putStrLn ("Test " ++ testName ++ ": PASSED")
     else
      error ("Test " ++ testName ++ ": FAILED")

main :: IO ()
main
  = do check "invGate" invGateTest
       check "nandGate" nandGateTest
