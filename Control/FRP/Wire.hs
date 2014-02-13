{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |Core wire type, instances, and primitives.
module Control.FRP.Wire(Wire, stepWire) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Control.Arrow.Class.Rutten

import Control.Applicative
import Data.Monoid

-- |Basic FRP wire type. It's actually, in essence, a simple arrow automaton:
-- isomorphic to Automaton (->) a b. That is, it is roughly correspondant to:
--
-- >  type Wire a b = a -> (b, Wire a b)
newtype Wire a b = Wire (a -> (b, Wire a b))

instance Category Wire where
  id = Wire (\x -> (x, id))
  Wire f . Wire g = Wire h
    where h x = let (y, g') = g x
                    (z, f') = f y in (z, f' . g')

instance Arrow Wire where
  arr f = w
    where w = Wire (\x -> (f x, w))
  first (Wire f) = Wire g
    where g ~(x, k) = let (y, f') = f x in ((y, k), first f')
  second (Wire f) = Wire g
    where g ~(k, x) = let (y, f') = f x in ((k, y), second f')
  Wire f *** Wire g = Wire h
    where h ~(x, y) = let (x', f') = f x
                          (y', g') = g y in ((x', y'), f' *** g')
  Wire f &&& Wire g = Wire h
    where h x = let (y, f') = f x
                    (z, g') = g x in ((y, z), f' &&& g')

instance ArrowChoice Wire where
  left (Wire f) = Wire g
    where g (Left x)  = let (y, f') = f x in (Left y, left f')
          g (Right x) = (Right x, Wire g)
  right (Wire f) = Wire g
    where g (Left x)  = (Left x, Wire g)
          g (Right x) = let (y, f') = f x in (Right y, right f')
  Wire f +++ Wire g = Wire h
    where h (Left x)  = let (y, f') = f x in (Left y,  f' +++ Wire g)
          h (Right x) = let (y, g') = g x in (Right y, Wire f +++ g')
  Wire f ||| Wire g = Wire h
    where h (Left x)  = let (y, f') = f x in (y, f' ||| Wire g)
          h (Right x) = let (y, g') = g x in (y, Wire f ||| g')

instance ArrowLoop Wire where
  loop (Wire f) = Wire g
    where g x = let ((out, common), f') = f (x, common) in (out, loop f')

instance ArrowRutten Wire where
  delay x = Wire (\y -> (x, delay y))

instance Functor (Wire a) where
  fmap f = (>>^ f)

instance Applicative (Wire a) where
  pure x = w
    where w = Wire (\_ -> (x, w))
  f <*> x = (f &&& x) >>> arr (uncurry id)

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
stepWire (Wire w) = w
{-# INLINE stepWire #-}

