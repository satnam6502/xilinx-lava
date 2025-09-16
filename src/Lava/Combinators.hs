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

module Lava.Combinators (module Control.Monad,
       fork, loop, par, par2, halve, unhalve, pair, unpair, zipA, unzipA,
       riffle, unriffle, two, evens, ilv, vreg, pairLeft, zipArray, swap)
where
import Control.Monad.Fix (MonadFix)
import Control.Monad ((>=>))
import Data.Array.Shaped
import GHC.TypeLits
import Lava.Hardware

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

par :: (KnownNat n, Applicative m) => (a -> m b) -> Array '[n] a -> m (Array '[n] b)
par f a = traverseA id (mapA f a)

par2 :: Monad m => (a -> m b) -> (c -> m d) -> (a, c) -> m (b, d)
par2 f g (a, b)
  = do x <- f a
       y <- g b
       return (x, y)

halve :: forall a n m. (KnownNat n, Applicative m) => Array '[2 * n] a -> m (Array '[2] (Array '[n] a))
halve = pure . unravel . reshape @'[2, n]

unhalve :: forall a n m. (KnownNat n, Applicative m) => Array '[2] (Array '[n] a) -> m (Array '[2 * n] a)
unhalve = pure . reshape @'[2 * n] . ravel

pair :: forall a n m. (KnownNat n, Applicative m) => Array '[2 * n] a -> m (Array '[n] (Array '[2] a))
pair = pure . unravel . reshape @'[n, 2]

unpair :: forall a n m. (KnownNat n, Applicative m) => Array '[n] (Array '[2] a) -> m (Array '[2 * n] a)
unpair = pure . reshape @'[2 * n] . ravel

zipA :: (KnownNat n, Applicative m) => Array '[2] (Array '[n] a) -> m (Array '[n] (Array '[2] a))
zipA =  pure . unravel . transpose @[1, 0] . ravel

unzipA :: (KnownNat n, Applicative m) => Array '[n] (Array '[2] a) -> m (Array '[2] (Array '[n] a))
unzipA =  pure . unravel . transpose @[1, 0] . ravel

riffle :: (KnownNat n, Monad m) => Array '[2 * n] a -> m (Array '[2 * n] a)
riffle = halve >=> zipA >=> unpair

unriffle :: (KnownNat n, Monad m) => Array '[2 * n] a -> m (Array '[2 * n] a)
unriffle = pair >=> unzipA >=> unhalve

two :: (KnownNat n, Monad m) => (Array '[n] a ->  m (Array '[n] a)) -> Array '[2 * n] a -> m (Array '[2 * n] a)
two f = halve >=> par f >=> unhalve

evens ::(KnownNat n, Monad m) => (Array '[2] a -> m (Array '[2] a)) -> Array '[n * 2] a -> m (Array '[n * 2] a)
evens f = pair >=> par f >=> unpair

ilv :: (KnownNat n, Monad m) => (Array '[n] a -> m (Array '[n] a)) -> Array '[n * 2] a -> m (Array '[n * 2] a)
ilv r = unriffle >=> two r >=> riffle

vreg :: (Hardware m bit, KnownNat n) => Array '[n] bit -> m (Array '[n] bit)
vreg = par reg

pairLeft :: Applicative m => a -> b -> m (b, a)
pairLeft a b = pure (b, a)

zipArray :: (Applicative m, KnownNat n) => (Array '[n] a, Array '[n] b) -> m (Array '[n] (a, b))
zipArray (a, b) = pure $ zipWithA (,) a b

swap :: Applicative m => (a, b) -> m (b, a)
swap (a, b) = pure (b, a)