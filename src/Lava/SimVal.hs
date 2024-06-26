{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Lava.SimVal where

import qualified Data.BitVector as BV

l :: SimVal Bool
l = L

h :: SimVal Bool
h = H

data SimVal a where
   L :: SimVal Bool
   H :: SimVal Bool
   BitVec :: BV.BitVector -> SimVal [Bool]
   Vec :: [SimVal a] -> SimVal [a]

deriving stock instance Show (SimVal a)

boolVec :: [Bool] -> SimVal [Bool]
boolVec v = Vec (map boolToSimVal v)

boolToSimVal :: Bool -> SimVal Bool
boolToSimVal False = L
boolToSimVal True = H

class SimValClass a b where
   toSimVal :: a -> SimVal b

instance SimValClass Int Bool where
   toSimVal :: Int -> SimVal Bool
   toSimVal 0 = L
   toSimVal 1 = H
   toSimVal other = error ("toSimVal Int Bool is only defined for 0 and 1, not " ++ show other)

