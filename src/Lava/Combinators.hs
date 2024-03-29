-- | Generic combinators for system construction.

module Lava.Combinators
where

-- | Fork captures sharing.
fork :: Monad m => a -> m (a, a)
fork a = return (a, a)