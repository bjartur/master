module Bjartur.Time where

import Data.Function( (&) )
import Data.Functor( (<&>) )
import Data.List( sort )
import Test.QuickCheck ( Arbitrary, arbitrary, choose )
import Data.Interval( Boundary(Closed), Interval,interval, lowerBound, upperBound )
import Data.IntervalSet

type Event  = Interval DateTime
type Events = IntervalSet DateTime

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

period :: Ord r=> r-> r-> Interval r
period start end=
  interval
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
      then error $ "Bjartur.Time: Time measurement across months not yet implemented: " ++ show from ++ " ... " ++ show to
      else zipWith seconds accessors units & sum

measure :: Event -> Int
measure = imperiod <&> measure'

imperiod :: Event -> (DateTime, DateTime)
imperiod event = do
  let (Finite lower) = lowerBound event
  let (Finite upper) = upperBound event
  (lower, upper)

imperiods:: Events-> [(DateTime, DateTime)]
imperiods= toAscList <&> map imperiod

nubSort:: Ord a=> [a]-> [a]
nubSort = sort <&> fastnub
    where fastnub(one:other:rest) = if one==other then fastnub(one:rest) else one:fastnub(other:rest)
          fastnub short = short
