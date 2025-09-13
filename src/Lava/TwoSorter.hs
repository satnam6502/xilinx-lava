{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lava.TwoSorter where

import Lava
import Data.Array.Shaped
import Lava.Mux
import Lava.Sub4

twoSorter :: Hardware m bit => (Array '[4] bit, Array '[4] bit) ->
                               m (Array '[4] bit, Array '[4] bit)
twoSorter (a, b)
  = do (_, cout1) <- sub4 a b
       x <- muxN cout1 (a, b)
       (_, cout2) <- sub4 b a
       y <- muxN cout2 (a, b)
       return (x, y)

twoSorterTop :: RTL ()
twoSorterTop
  = do setModuleName "twoSorter"
       a :: Array '[4] (Net Bit) <- inputVec "a" BitType
       b :: Array '[4] (Net Bit) <- inputVec "b" BitType
       (c, d) <- twoSorter (a, b)
       outputVec "c" c BitType
       outputVec "d" d BitType

twoSorterA :: Hardware m bit => Array '[2] (Array '[4] bit) -> m (Array '[2] (Array '[4] bit))
twoSorterA a
  = do (p, q) <- twoSorter (x, y)
       return (fromList [p, q])
    where
    x = unScalar (index a 0)
    y = unScalar (index a 1)

twoSorterReg :: Hardware m bit => Array '[2] (Array '[4] bit) -> m (Array '[2] (Array '[4] bit))
twoSorterReg = twoSorterA >=> par vreg

twoSorterRegTop :: RTL ()
twoSorterRegTop
  = do setModuleName "twoSorterReg"
       setClockNet "clk"
       setActiveLowResetNet "rstN"
       a :: Array '[4] (Net Bit) <- inputVec "a" BitType
       b :: Array '[4] (Net Bit) <- inputVec "b" BitType
       cd <- twoSorterReg (fromList [a, b])
       let cdL = toList cd
       outputVec "c" (cdL!!0) BitType
       outputVec "d" (cdL!!1) BitType
