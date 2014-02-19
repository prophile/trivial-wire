-- |Signal convenience type.
module Control.FRP.Signal(Signal) where

-- |Signals in an arrow.
type Signal a b c = a (Maybe b) (Maybe c)

