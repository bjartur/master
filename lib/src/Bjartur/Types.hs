module Bjartur.Types where

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

-- Represent number of lines in a file
type PathLines = (FilePath, Int)
