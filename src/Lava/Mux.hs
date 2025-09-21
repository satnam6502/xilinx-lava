{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lava.Mux where

import Lava
import Data.Array.Shaped
import GHC.TypeLits


oneBitMux :: Hardware m bit => bit -> (bit, bit) -> m bit
oneBitMux sel (a, b) = lut3 (\s x y -> (not s && x) || (s && y)) (sel, a, b)


muxN :: (KnownNat n, Hardware m bit) => bit -> (Array '[n] bit, Array '[n] bit) -> m (Array '[n] bit)
muxN sel (aV, bV)
  -- = -- traverseA id (zipWithA (curry (oneBitMux sel)) aV bV)
  = vmap (oneBitMux sel) (zipWithA (,) aV bV)


muxN8Top :: RTL ()
muxN8Top
  = do setModuleName "muxN8"
       sel <- input "sel" BitType
       a :: Array '[8] (Net Bit) <- inputVec "a" BitType
       b :: Array '[8] (Net Bit) <- inputVec "b" BitType
       c :: Array '[8] (Net Bit) <- muxN sel (a, b)
       outputVec "c" c BitType
