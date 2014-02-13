-- |Arrows with single-element delays, a la Jan Rutten's streams.
module Control.Arrow.Class.Rutten(ArrowRutten, delay) where

import Control.Arrow(Arrow)

-- |Arrows which can delay an element.
class (Arrow a) => ArrowRutten a where
  -- |Delay an element, returning the argument the first time.
  delay :: v -> a v v

