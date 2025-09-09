{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lava.Mux where

import Lava.Hardware
import Lava.RTL
import Data.Array.Shaped
import GHC.TypeLits


oneBitMux :: Hardware m bit => bit -> (bit, bit) -> m bit
oneBitMux sel (a, b) = lut3 (\s x y -> ((not s) && x) || (s && y)) (sel, a, b)


muxN :: (KnownNat n, Hardware m bit) => bit -> (Array '[n] bit, Array '[n] bit) -> m (Array '[n] bit)
muxN sel (aV, bV)
  = traverseA id (zipWithA (curry (oneBitMux sel)) aV bV)


muxN8Top :: RTL ()
muxN8Top
  = do setModuleName "muxN8"
       sel <- input "sel" BitType
       a :: Array '[8] (Net Bit) <- inputVec "a" BitType
       b :: Array '[8] (Net Bit) <- inputVec "b" BitType
       c :: Array '[8] (Net Bit) <- muxN sel (a, b)
       outputVec "c" c BitType
