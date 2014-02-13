{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.FRP.Wire(Wire, stepWire) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Automaton

import Control.Applicative(Applicative)

newtype Wire a b = Wire (Automaton (->) a b)
  deriving (Category, Arrow,
            ArrowChoice,
            ArrowLoop, ArrowCircuit,
            Functor, Applicative)

stepWire :: Wire a b -> a -> (b, Wire a b)
stepWire (Wire (Automaton f)) x = let (res, wire') = f x in (res, Wire wire')
{-# INLINE stepWire #-}

