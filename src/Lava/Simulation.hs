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

instance Hardware Sim [Bool] where
  inv :: [Bool] -> Sim [Bool]
  inv = invSim
  and2 :: ([Bool], [Bool]) -> Sim [Bool]
  and2 = and2Sim


