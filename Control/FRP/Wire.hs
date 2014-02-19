{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |Core wire type, instances, and primitives.
module Control.FRP.Wire(Wire, stepWire) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Control.Arrow.Operations(ArrowCircuit)
import Control.Arrow.Transformer(lift)
import Control.Arrow.Transformer.Automaton(Automaton(Automaton))

import Control.Arrow.Improve(ImproveArrow, lowerImprove)

-- |Basic FRP wire type. It's actually, in essence, a simple arrow automaton:
-- isomorphic to Automaton (->) a b. That is, it is roughly correspondant to:
--
-- >  type Wire a b = a -> (b, Wire a b)
newtype BaseWire a b = BaseWire (Automaton (->) a b)
  deriving (Category, Arrow,
            ArrowChoice,
            ArrowLoop, ArrowCircuit)

type Wire a b = ImproveArrow BaseWire a b

-- |Run a single step of a wire with inputs, getting both the current outputs
-- and the next iteration of the wire to run.
stepBaseWire :: BaseWire a b -> a -> (b, BaseWire a b)
stepBaseWire (BaseWire (Automaton f)) x = (res, BaseWire wire')
  where ~(res, wire') = f x
{-# INLINE stepBaseWire #-}

-- |Run a single step of a wire with inputs, getting both the current outputs
-- and the next iteration of the wire to run.
stepWire :: Wire a b -> a -> (b, Wire a b)
stepWire w input = (res, lift wire')
  where ~(res, wire') = stepBaseWire (lowerImprove w) input
{-# INLINE stepWire #-}

