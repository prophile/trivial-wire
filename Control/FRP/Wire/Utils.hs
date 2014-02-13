{-# LANGUAGE Arrows #-}

module Control.FRP.Wire.Utils(viewWire, runWire, runWireTerminal,
                              accumulate, wsum, wproduct) where

import Control.FRP.Wire
import Data.Monoid
import Control.Arrow
import Control.Arrow.Operations(ArrowCircuit, delay)

viewWire :: Wire i o -> [i] -> [o]
viewWire _ [] = []
viewWire w (i:is) = let (o, w') = stepWire w i in o:viewWire w' is

runWire :: (Monad m) => Wire i o -> m i -> (o -> m ()) -> m ()
runWire w i o = descend w
  where descend wire = do input <- i
                          let (output, nextStep) = stepWire wire input
                          o output
                          descend nextStep

runWireTerminal :: Wire String String -> IO ()
runWireTerminal w = runWire w getLine putStrLn

accumulate :: (ArrowCircuit a, Monoid m) => a m m
accumulate = proc x -> do rec prevTotal <- delay mempty -< newTotal
                              let newTotal = prevTotal `mappend` x
                          returnA -< newTotal

wsum :: (ArrowCircuit a, Num n) => a n n
wsum = Sum ^>> accumulate >>^ getSum

wproduct :: (ArrowCircuit a, Num n) => a n n
wproduct = Product ^>> accumulate >>^ getProduct

