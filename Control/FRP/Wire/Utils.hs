module Control.FRP.Wire.Utils(viewWire, runWire, runWireTerminal) where

import Control.FRP.Wire

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

