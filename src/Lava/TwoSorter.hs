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

twoSorter ::  Hardware m bit => (Array '[4] bit, Array '[4] bit) ->
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
