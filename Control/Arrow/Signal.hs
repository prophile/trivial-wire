module Control.Arrow.Signal(liftSignal) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow

liftSignal :: (ArrowChoice a) => a b (Maybe c) -> a (Maybe b) (Maybe c)
liftSignal a = arr cFromMaybe >>> right a >>> arr cToMaybe
  where cFromMaybe Nothing  = Left ()
        cFromMaybe (Just x) = Right x
        cToMaybe (Left ())        = Nothing
        cToMaybe (Right Nothing)  = Nothing
        cToMaybe (Right (Just x)) = Just x

