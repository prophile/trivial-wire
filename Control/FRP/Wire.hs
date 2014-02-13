{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |Core wire type, instances, and primitives.
module Control.FRP.Wire(Wire, stepWire) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Automaton

import Control.Applicative
import Data.Monoid

-- |Basic FRP wire type. It's actually, in essence, a simple arrow automaton:
-- isomorphic to Automaton (->) a b. That is, it is roughly correspondant to:
--
-- >  type Wire a b = a -> (b, Wire a b)
newtype Wire a b = Wire (Automaton (->) a b)
  deriving (Category, Arrow,
            ArrowChoice,
            ArrowLoop, ArrowCircuit,
            Functor, Applicative)

instance (Monoid b) => Monoid (Wire a b) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance (Num b) => Num (Wire a b) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)

  negate = fmap negate

  signum = fmap signum
  abs    = fmap abs

  fromInteger = pure . fromInteger

instance (Fractional b) => Fractional (Wire a b) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Floating b) => Floating (Wire a b) where
  pi      = pure pi
  exp     = fmap exp
  sqrt    = fmap sqrt
  log     = fmap log
  (**)    = liftA2 (**)
  logBase = liftA2 logBase
  sin     = fmap sin
  tan     = fmap tan
  cos     = fmap cos
  asin    = fmap asin
  atan    = fmap atan
  acos    = fmap acos
  sinh    = fmap sinh
  tanh    = fmap tanh
  cosh    = fmap cosh
  asinh   = fmap asinh
  atanh   = fmap atanh
  acosh   = fmap acosh

instance (Bounded b) => Bounded (Wire a b) where
  minBound = pure minBound
  maxBound = pure maxBound

-- |Run a single step of a wire with inputs, getting both the current outputs
-- and the next iteration of the wire to run.
stepWire :: Wire a b -> a -> (b, Wire a b)
stepWire (Wire (Automaton f)) x = let (res, wire') = f x in (res, Wire wire')
{-# INLINE stepWire #-}

