module Bjartur.Time where

import Data.Function( (&) )
import Data.Functor( (<&>) )
import Data.List( foldl1' )
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

flatten :: [(DateTime, DateTime)]-> [DateTime]
flatten [] = []
flatten ((a,b):xs) = a:b:flatten xs

merge :: [DateTime]-> [DateTime]-> [DateTime]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) = merge' a as b bs (compare a b)

merge' :: DateTime-> [DateTime]-> DateTime-> [DateTime]-> Ordering-> [DateTime]
merge' a as _ bs EQ = a:merge as bs
merge' a as b bs LT = a:merge as (b:bs)
merge' a as b bs GT = b:merge (a:as) bs

mergeAll :: [[DateTime]]-> [DateTime]
mergeAll [] = []
mergeAll [x] = x
mergeAll (as:bs:rest) = mergeAll ((merge as bs):rest)

flattenAll :: [[(DateTime,DateTime)]] -> [DateTime]
flattenAll xs = map flatten xs & mergeAll

telja :: [DateTime]-> [(DateTime, DateTime)]-> [Bool]
telja [] [] = []
telja [] datetime = error $ "DateTime " ++ show datetime ++ " out of bounds!"
telja (_:allir) [] = False : telja allir []
telja (núverandi:allir) ((upphaf,endir):eini) = if núverandi < upphaf then False : telja allir ((upphaf,endir):eini) else True : teljaTrue allir ((upphaf,endir):eini)


teljaTrue :: [DateTime]-> [(DateTime, DateTime)]-> [Bool]
teljaTrue [] _ = error "Closing datetime lost!"
teljaTrue _ [] = error "teljaTrue called without the open interval!"
teljaTrue (núverandi:allir) ((_,endir):eini) = if núverandi < endir then True : teljaTrue allir ((undefined,endir):eini) else False : telja allir eini

samtelja :: [[(DateTime,DateTime)]]-> [Int]
samtelja [] = []
samtelja xs =  do
  let u = flattenAll xs & telja
  let v = map u xs ::[[Bool]]
  v <&> map fromEnum & foldl1' (zipWith (+))
