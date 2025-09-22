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
  = do (_, cout1) <- sub4 (a, b)
       x <- muxN cout1 (a, b)
       (_, cout2) <- sub4 (b, a)
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

twoSorterCombinatonal :: Hardware m bit => Array '[2] (Array '[4] bit) -> m (Array '[2] (Array '[4] bit))
twoSorterCombinatonal i
  = do (c, d) <- twoSorter (a, b)
       return (fromList [c, d])
    where
    a = unScalar (i `index` 0)
    b = unScalar (i `index` 1)

-- A pipelined version of the twoSorter
twoSorterA :: Hardware m bit => Array '[2] (Array '[4] bit) -> m (Array '[2] (Array '[4] bit))
twoSorterA a
  = do (p, q) <- twoSorterRegL (x, y)
       return (fromList [p, q])
    where
    x = unScalar (index a 0)
    y = unScalar (index a 1)

twoSorterReg :: Hardware m bit => (Array '[4] bit, Array '[4] bit) ->
                               m (Array '[4] bit, Array '[4] bit)
twoSorterReg = twoSorter >=> par2 (traverseA reg) (traverseA reg)

twoSorterRegTop :: RTL ()
twoSorterRegTop
  = do setModuleName "twoSorterReg"
       setClockNet "clk"
       setActiveLowResetNet "rstN"
       a :: Array '[4] (Net Bit) <- inputVec "a" BitType
       b :: Array '[4] (Net Bit) <- inputVec "b" BitType
       (c, d) <- twoSorterRegL (a, b)
       outputVec "c" c BitType
       outputVec "d" d BitType

twoSorterRegL :: Hardware m bit => (Array '[4] bit, Array '[4] bit) -> m (Array '[4] bit, Array '[4] bit)
twoSorterRegL (a, b) = (fork >=> vpar2 (         sub4OnlyCarryOut >-> (mux >|> vmap reg))
                                       (swap >=> sub4OnlyCarryOut >-> (mux >|> vmap reg))) (a, b)
            where
            mux s = muxN s (a, b)

max2 :: Hardware m bit => (Array '[4] bit, Array '[4] bit) -> m (Array '[4] bit)
max2 (a, b) = (sub4OnlyCarryOut >-> (mux >|> vmap reg)) (a, b)
       where
       mux s = muxN s (a, b)

max2Top :: RTL ()
max2Top
  = do setModuleName "max2"
       setClockNet "clk"
       setActiveLowResetNet "rstN"
       a :: Array '[4] (Net Bit) <- inputVec "a" BitType
       b :: Array '[4] (Net Bit) <- inputVec "b" BitType
       c <- max2 (a, b)
       outputVec "c" c BitType

  