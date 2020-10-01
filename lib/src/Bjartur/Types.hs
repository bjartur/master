module Bjartur.Types where

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

-- Represent number of lines in a file
type PathLines = (FilePath, Int)
