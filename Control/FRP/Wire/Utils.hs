-- |Non-primitive wire utilities, including useful testing and debugging tools.
module Control.FRP.Wire.Utils(viewWire, runWire, runWireTerminal,
                              accumulate, wsum, wproduct,
                              wdecisive, wlatch,
                              differentiate) where

import Prelude hiding ((.), id)
import Control.Category

import Control.FRP.Wire
import Data.Monoid
import Control.Arrow

import Control.Arrow.Class.Rutten

-- |Run a wire with a list of inputs mapped to a list of outputs. Useful for
-- tests, probably not hugely useful in practical code.
viewWire :: Wire i o -> [i] -> [o]
viewWire _ [] = []
viewWire w (i:is) = let (o, w') = stepWire w i in o:viewWire w' is

-- |Run a wire forever within a monad, with monadic methods for receiving input
-- and putting output.
runWire :: (Monad m) => Wire i o -> m i -> (o -> m ()) -> m ()
runWire w i o = descend w
  where descend wire = do input <- i
                          let (output, nextStep) = stepWire wire input
                          o output
                          descend nextStep

-- |Specific form of 'runWire' for reading and writing strings from stdout and
-- stderr. Useful for testing wires in a REPL.
runWireTerminal :: Wire String String -> IO ()
runWireTerminal w = runWire w getLine putStrLn

-- |Generic integration combinator, generalised to any 'ArrowRutten'. For those
-- who enjoy such things, this can be considered as being 1 / (1 - z^-1).
accumulate :: (ArrowLoop a, ArrowRutten a, Monoid m) => a m m
accumulate = loop $ second (delay mempty) >>^ process
  where process (x, prevTotal) = let newTotal = prevTotal `mappend` x
                                   in (newTotal, newTotal)

-- |Specialisation of 'accumulate' for numerical summation. This is, in essence,
-- integration.
wsum :: (ArrowLoop a, ArrowRutten a, Num n) => a n n
wsum = Sum ^>> accumulate >>^ getSum

-- |Specialisation of 'accumulate' for products.
wproduct :: (ArrowLoop a, ArrowRutten a, Num n) => a n n
wproduct = Product ^>> accumulate >>^ getProduct

-- |Specialisation of 'accumulate' to latch to the first Just that it sees.
wdecisive :: (ArrowLoop a, ArrowRutten a) => a (Maybe b) (Maybe b)
wdecisive = First ^>> accumulate >>^ getFirst

-- |Specialisation of 'accumulate' to latch to the latest Just that it has seen.
wlatch :: (ArrowLoop a, ArrowRutten a) => a (Maybe b) (Maybe b)
wlatch = Last ^>> accumulate >>^ getLast

-- |Straightforward differentiation, for numerical differentiation use 0 and (-)
-- as the arguments, which yields 1 - z^-1.
differentiate :: (ArrowRutten a) => b -> (b -> b -> c) -> a b c
differentiate z c = (id &&& delay z) >>> arr (uncurry c)

