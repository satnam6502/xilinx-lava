{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
module Lava.Simulation where
import Control.Monad.Identity
import Lava.Hardware
import Data.Array.Shaped

type Sim = Identity

runSim :: Sim a -> a
runSim = runIdentity

invSim :: [Bool] -> Identity [Bool]
invSim x = return (map not x)

simBinOp :: (Bool -> Bool -> Bool) -> [[Bool]] -> Sim [Bool]
simBinOp _ [] = error "Binary gate given an empty list of inputs"
simBinOp _ [_] = error "Binary gate given just one input"
simBinOp f xs = simBinOp' f xs

simBinOp' :: (Bool -> Bool -> Bool) -> [[Bool]] -> Sim [Bool]
simBinOp' f [a, b] = return (zipWith f a b)
simBinOp' f (a:b:xs) = do rhs <- simBinOp' f (b:xs)
                          simBinOp' f [a, rhs]
simBinOp' _ _ = error "bad simBinOp' case"

xnor2Sim :: ([Bool], [Bool]) -> Sim [Bool]
xnor2Sim (x, y) = return (zipWith (==) x y)

instance Hardware Sim [Bool] where
  zero :: Sim [Bool]
  zero = return (repeat False)
  one :: Sim [Bool]
  one = return (repeat True)
  invGate :: [Bool] -> Sim [Bool]
  invGate = invSim
  andGate :: [[Bool]] -> Sim [Bool]
  andGate = simBinOp (&&)
  orGate :: [[Bool]] -> Sim [Bool]
  orGate = simBinOp (||)
  xorGate :: [[Bool]] -> Sim [Bool]
  xorGate = simBinOp' (/=)
  norGate :: [[Bool]] -> Sim [Bool]
  norGate =  simBinOp (\a b -> not (a || b))
  xnorGate :: [[Bool]] -> Sim [Bool]
  xnorGate = simBinOp (==)
  delay :: [Bool] -> Sim [Bool]
  delay xs = return (False : xs)
  xorcy ::  ([Bool], [Bool]) -> Sim [Bool]
  xorcy (a, b) = xorGate [a, b]
  muxcy :: ([Bool], ([Bool], [Bool])) -> Sim [Bool]
  muxcy (s, (ci, di)) = return [if s' then ci' else di' | (s', (ci', di')) <- zip s (zip ci di)]
  carry4 :: [Bool] -> [Bool] -> Array '[4] [Bool] -> Array '[4] [Bool] -> Sim (Array '[4] [Bool], Array '[4] [Bool])
  carry4 = carry4ArraySim
  lut2 :: (Bool -> Bool -> Bool) -> ([Bool], [Bool]) -> Sim [Bool]
  lut2 f (i0, i1) = return [f a b | (a, b) <- zip i0 i1]
  lut3 :: (Bool -> Bool -> Bool -> Bool) -> ([Bool], [Bool], [Bool]) -> Sim [Bool]
  lut3 f (i0, i1, i2) = return [f a b c | (a, b, c) <- zip3 i0 i1 i2]
  reg :: [Bool] -> Sim [Bool]
  reg = delay

carry4ArraySim:: [Bool] -> [Bool] -> Array '[4] [Bool] -> Array '[4] [Bool] -> Sim (Array '[4] [Bool], Array '[4] [Bool])
carry4ArraySim ci cyinit di s
  =  do (o, co) <- carry4Sim ci cyinit (diL!!0, diL!!1, diL!!2, diL!!3) (sL!!0, sL!!1, sL!!2, sL!!3)
        let (o0, o1, o2, o3) = o
            (co0, co1, co2, co3) = co
        return (fromList [o0, o1, o2, o3], fromList [co0, co1, co2, co3])
     where
     diL = toList di
     sL = toList s
carry4Sim :: [Bool] -> [Bool] -> ([Bool], [Bool], [Bool], [Bool]) -> ([Bool], [Bool], [Bool], [Bool]) ->
            Sim (([Bool], [Bool], [Bool], [Bool]), ([Bool], [Bool], [Bool], [Bool]))
carry4Sim ci cyinit di s
  = return (untransposeCarry4 carry4Out)
    where
    carry4Out = [carry4sim ci' cyinit' di' s' | (ci', cyinit', di', s') <- transposeCarry4 ci cyinit di s]

transposeCarry4 :: [Bool] -> [Bool] -> ([Bool], [Bool], [Bool], [Bool]) -> ([Bool], [Bool], [Bool], [Bool]) ->
                   [(Bool, Bool, (Bool, Bool, Bool, Bool), (Bool, Bool, Bool, Bool))]
transposeCarry4 bs1 bs2 (as1, as2, as3, as4) (cs1, cs2, cs3, cs4) = 
    zipWith4 (\b1 b2 a4tuple c4tuple -> (b1, b2, a4tuple, c4tuple))
             bs1 bs2 (zipWith4 (,,,) as1 as2 as3 as4) (zipWith4 (,,,) cs1 cs2 cs3 cs4)

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _ = []

untransposeCarry4 :: [((Bool, Bool, Bool, Bool), (Bool, Bool, Bool, Bool))] ->
                     (([Bool], [Bool], [Bool], [Bool]), ([Bool], [Bool], [Bool], [Bool]))
untransposeCarry4 xs = (transpose4 firsts, transpose4 seconds)
  where
    (firsts, seconds) = unzip xs
    transpose4 ts = ( [a | (a,_,_,_) <- ts]
                    , [b | (_,b,_,_) <- ts]
                    , [c | (_,_,c,_) <- ts]
                    , [d | (_,_,_,d) <- ts]
                    )

carry4sim :: Bool -> Bool -> (Bool, Bool, Bool, Bool) -> (Bool, Bool, Bool, Bool) -> ((Bool, Bool, Bool, Bool), (Bool, Bool, Bool, Bool))
carry4sim cyinit ci (di0,di1,di2,di3) (s0,s1,s2,s3) =
  ((co0,co1,co2,co3), (o0,o1,o2,o3))
  where
    -- Carry input for bit 0 (CYINIT takes precedence over CI)
    ci0 = cyinit || ci

    -- Bit 0
    co0 = (di0 && not s0) || (ci0 && s0)
    o0  = s0 /= ci0  -- XOR

    -- Bit 1  
    co1 = (di1 && not s1) || (co0 && s1)
    o1  = s1 /= co0  -- XOR

    -- Bit 2
    co2 = (di2 && not s2) || (co1 && s2)
    o2  = s2 /= co1  -- XOR

    -- Bit 3
    co3 = (di3 && not s3) || (co2 && s3)
    o3  = s3 /= co2  -- XOR

