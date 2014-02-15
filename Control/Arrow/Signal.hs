{-# LANGUAGE Arrows #-}

module Control.Arrow.Signal(liftSignal) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow

liftSignal :: (ArrowChoice a) => a b (Maybe c) -> a (Maybe b) (Maybe c)
liftSignal a = proc x -> case x of
                           Nothing -> id -< Nothing
                           Just x -> a -< x

