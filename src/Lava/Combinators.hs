{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic combinators for system construction.

module Lava.Combinators (module Control.Monad, fork, loop, par)
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

