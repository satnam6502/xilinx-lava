module Main (main) where

import Lava
import Lava.Examples.NandGate
import Lava.OneBitAdder
import Data.List (transpose)

--------------------------------------------------------------------------------

invGateTest :: Bool
invGateTest = runSim (inv [False, True]) == map not [False, True]

--------------------------------------------------------------------------------

pair2tuple :: [a] -> (a, a)
pair2tuple [x, y] = (x, y)
pair2tuple _ = error "pair2tuple"

bool2Inputs :: ([Bool], [Bool])
bool2Inputs = pair2tuple $ transpose [[x, y] | x <- [False, True], y <- [False, True]]


triple2tuple' :: [a] -> (a, (a, a))
triple2tuple' [s, x, y] = (s, (x, y))
triple2tuple' _ = error "triple2tuple'"

bool3Inputs' :: ([Bool], ([Bool], [Bool]))
bool3Inputs' = triple2tuple' $ transpose [[cin, x, y] | cin <- [False, True], x <- [False, True], y <- [False, True]]

--------------------------------------------------------------------------------

nandGateGolden :: ([Bool], [Bool]) -> [Bool]
nandGateGolden (xs, ys) = [not (x && y) | (x, y) <- zip xs ys]

nandGateTest :: Bool
nandGateTest = runSim (nandGate bool2Inputs) ==
               nandGateGolden bool2Inputs

--------------------------------------------------------------------------------

oneBitAdderGolden ::  ([Bool], ([Bool], [Bool])) -> ([Bool], [Bool])
oneBitAdderGolden (cin, (a, b))
  = (map fst sumAndCout, map snd sumAndCout)
     where
     sumAndCout = [((cin' /= a') /= b', if a' /= b' then a' else cin') | (cin', (a', b')) <- zip cin (zip a b)]

oneBitAdderTest :: Bool
oneBitAdderTest = runSim (oneBitAdder bool3Inputs') ==
                  oneBitAdderGolden bool3Inputs'

--------------------------------------------------------------------------------

check :: String -> Bool -> IO ()
check testName passed
  = if passed then
      putStrLn ("Test " ++ testName ++ ": PASSED")
     else
      error ("Test " ++ testName ++ ": FAILED")

--------------------------------------------------------------------------------

main :: IO ()
main
  = do check "invGate" invGateTest
       check "nandGate" nandGateTest
       check "oneBitAdder" oneBitAdderTest
