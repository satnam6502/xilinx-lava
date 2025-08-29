{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Lava.Simulation where
import Control.Monad.Identity
import Lava.Hardware

type Sim = Identity

runSim :: Sim a -> a
runSim = runIdentity

invSim :: [Bool] -> Identity [Bool]
invSim x = return (map not x)

and2Sim :: ([Bool], [Bool]) -> Identity [Bool]
and2Sim (x, y) = return (map (uncurry (&&)) (zip x y))

or2Sim :: ([Bool], [Bool]) -> Identity [Bool]
or2Sim (x, y) = return (map (uncurry (||)) (zip x y))

xor2Sim :: ([Bool], [Bool]) -> Identity [Bool]
xor2Sim (x, y) = return (map (uncurry (/=)) (zip x y))

nor2Sim :: ([Bool], [Bool]) -> Identity [Bool]
nor2Sim (x, y) = return (map (\(a, b) -> not (a || b)) (zip x y))

xnor2Sim :: ([Bool], [Bool]) -> Identity [Bool]
xnor2Sim (x, y) = return (map (uncurry (==)) (zip x y))

instance Hardware Sim [Bool] where
  inv :: [Bool] -> Sim [Bool]
  inv = invSim
  and2 :: ([Bool], [Bool]) -> Sim [Bool]
  and2 = and2Sim
  or2 :: ([Bool], [Bool]) -> Sim [Bool]
  or2 = or2Sim
  xor2 :: ([Bool], [Bool]) -> Sim [Bool]
  xor2 = xor2Sim
  nor2 :: ([Bool], [Bool]) -> Sim [Bool]
  nor2 = nor2Sim
  xnor2 :: ([Bool], [Bool]) -> Sim [Bool]
  xnor2 = xnor2Sim
  delay :: [Bool] -> Sim [Bool]
  delay xs = return (False : xs)
  xorcy ::  ([Bool], [Bool]) -> Sim [Bool]
  xorcy = xor2Sim
  muxcy :: ([Bool], ([Bool], [Bool])) -> Sim [Bool]
  muxcy (s, (ci, di)) = return [if s' then ci' else di' | (s', (ci', di')) <- zip s (zip ci di)]

