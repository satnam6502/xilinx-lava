{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generic combinators for system construction.

module Lava.Combinators (module Control.Monad, fork, loop, par, halve, unhalve, pair, unpair, zipA)
where
import Control.Monad.Fix (MonadFix)
import Control.Monad ((>=>)) 
import Data.Array.Shaped
import GHC.TypeLits

-- | Fork captures sharing. The input wire `a` is split
--   into two wires, each containing the value of `a`.
fork :: Monad m => a -> m (a, a)
fork a = return (a, a)

-- | Loop is used to bend a wire `feedback` from the output of a circuit
--   and feed it back as in input to a circuit. This requires
--   the feedback path to contain at least one state (delay)
--   element, otherwise the output will not terminate.
loop :: MonadFix m => ((a, feedback) -> m (b, feedback)) ->
                      a -> m b
loop circuit a
  = mdo (c, feedback) <- circuit (a, feedback)
        return c

par :: (KnownNat n, Monad m) => (a -> m b) -> Array '[n] a -> m (Array '[n] b)
par f a
  = traverseA id (mapA f a)


halve :: forall a n. (KnownNat n, KnownNat (2*n)) => Array '[2 * n] a -> Array '[2] (Array '[n] a)
halve = unravel . reshape @'[2, n]

unhalve :: forall a n. KnownNat n => Array '[2] (Array '[n] a) -> Array '[2 * n] a
unhalve = reshape @'[2 * n] . ravel

pair :: forall a n. KnownNat n => Array '[2 * n] a -> Array '[n] (Array '[2] a)
pair = unravel . reshape @'[n, 2]

unpair :: forall a n. KnownNat n => Array '[n] (Array '[2] a) -> Array '[2 * n] a
unpair = reshape @'[2 * n] . ravel

zipA :: forall a n. KnownNat n => Array '[2] (Array '[n] a) -> Array '[n] (Array '[2] a)
zipA =  unravel . transpose @[1, 0] . ravel