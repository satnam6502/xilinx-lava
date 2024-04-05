{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
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
   Vec :: [SimVal a] -> SimVal ([a])

deriving stock instance Show (SimVal a)

boolVec :: [Bool] -> SimVal [Bool]
boolVec v = Vec (map boolToSimVal v)

boolToSimVal :: Bool -> SimVal Bool
boolToSimVal False = L
boolToSimVal True = H
