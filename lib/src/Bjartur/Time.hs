module Bjartur.Time where

import Data.Function( (&) )
import Data.Functor( (<&>) )
import Test.QuickCheck ( Arbitrary, arbitrary, choose )
import qualified Data.Interval     as Interval
import qualified Data.IntervalSet  as IntervalSet
import Data.Interval (Extended(Finite), Boundary(Closed))

type Interval  = Interval.Interval DateTime
type Intervals = IntervalSet.IntervalSet DateTime

data DateTime= DateTime {
 year:: Int,
 month:: Int,
 day:: Int,
 hour:: Int,
 minute:: Int,
 second:: Int
} deriving (Eq, Ord, Show)

instance Arbitrary DateTime where
  arbitrary= pure DateTime
     <*> choose(1,9999)
     <*> choose(1,12)
     <*> choose(1,28)
     <*> choose(1,24)
     <*> choose(1,60)
     <*> choose(1,60)

period :: Ord r=> r-> r-> Interval.Interval r
period start end=
  Interval.interval
    (Finite start, Closed)
    (Finite end,   Closed)

measure' :: (DateTime, DateTime)-> Int
measure' (from, to)= let
  seconds accessor unit= unit * (accessor to - accessor from)
  increments= [1, 60, 60, 24, 365]
  accessors = [second, minute, hour, day, year]
  units= [1..length increments] <&> flip take increments <&> product
  in
  if month to - month from /= 0
      then error "Implausible data! Events overlapped for more than a month. Nobody sleeps that long."
      else zipWith seconds accessors units & sum

measure :: Interval -> Int
measure i = let (Finite lower) = Interval.lowerBound i
                (Finite upper) = Interval.upperBound i
            in measure' (lower, upper)
