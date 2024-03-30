{-# LANGUAGE RecursiveDo #-}

-- | Generic combinators for system construction.

module Lava.Combinators (module Control.Monad, fork, loop)
where
import Control.Monad.Fix (MonadFix)
import Control.Monad ((>=>)) 

-- | Fork captures sharing.
fork :: Monad m => a -> m (a, a)
fork a = return (a, a)

loop :: MonadFix m => ((a, feedback) -> m (b, feedback)) ->
                      a -> m b
loop circuit a
  = mdo (c, feedback) <- circuit (a, feedback)
        return c
