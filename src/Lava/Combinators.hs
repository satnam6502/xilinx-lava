{-# LANGUAGE RecursiveDo #-}

-- | Generic combinators for system construction.

module Lava.Combinators (module Control.Monad, fork, loop)
where
import Control.Monad.Fix (MonadFix)
import Control.Monad ((>=>)) 

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
