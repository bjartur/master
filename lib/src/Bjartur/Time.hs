module Bjartur.Time where

import Data.Function( (&) )
import Data.Functor( (<&>) )
import Data.List( sort, foldl1' )
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

flatten :: (Ord a, Show a)=> [(a, a)]-> [a]
flatten [] = []
flatten ((a,b):xs) = a:b:flatten xs

merge :: (Ord a, Show a)=> [a]-> [a]-> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) = merge' a as b bs (compare a b)

merge' :: (Ord a, Show a)=> a-> [a]-> a-> [a]-> Ordering-> [a]
merge' a as _ bs EQ = a:merge as bs
merge' a as b bs LT = a:merge as (b:bs)
merge' a as b bs GT = b:merge (a:as) bs

mergeAll :: (Ord a, Show a)=> [[a]]-> [a]
mergeAll [] = []
mergeAll [x] = x
mergeAll (as:bs:rest) = mergeAll ((merge as bs):rest)

flattenAll :: (Ord a, Show a)=> [[(a,a)]] -> [a]
flattenAll xs = map flatten xs & mergeAll

labelFromOutside :: (Ord a, Show a)=> [a]-> [(a, a)]-> [Bool]
labelFromOutside [] [] = []
labelFromOutside [_] [] = []
labelFromOutside [] datetime = error $ "DateTime " ++ show datetime ++ " out of bounds!"
labelFromOutside (_:a:llir) [] = False : labelFromOutside (a:llir) []
labelFromOutside (núverandi:allir) ((upphaf,endir):eini) = if núverandi < upphaf then False : labelFromOutside allir ((upphaf,endir):eini) else True : labelFromInside allir ((upphaf,endir):eini)


labelFromInside :: (Ord a, Show a)=> [a]-> [(a, a)]-> [Bool]
labelFromInside [] _ = error "Closing datetime lost!"
labelFromInside [_] [_]= []
labelFromInside _ [] = error "labelFromInside called without the open interval!"
labelFromInside (núverandi:allir) ((_,endir):eini) = if núverandi < endir then True : labelFromInside allir ((undefined,endir):eini) else False : labelFromOutside allir eini

-- inputs: List of lists of closed intervals and the ascending list of all of the endpoints of those intervals, without repetition.
-- output: For each pair of endpoints, how many of the intervals contain both endpoints?
-- The output is a list one shorter than the list of endpoints. It can be shorter or longer than the list of intervals.
count :: (Ord a, Show a)=> [[(a,a)]]-> [a]-> [Int]
count [] [] = []
count xs flati =  do
  let labelOne = labelFromOutside flati
  let allLabeled = map labelOne xs ::[[Bool]]
  let sane = all (length<&>(==length(allLabeled!!0))) allLabeled
  let value = allLabeled <&> map fromEnum & foldl1' (zipWith (+))
  if sane then value else error "count: Not all input lists of equal length!"

unflatten :: (Ord a, Show a)=> [a]-> [(a,a)]
unflatten xs = zip xs (tail xs)

flatCount :: (Ord a, Show a)=> [[(a,a)]]-> [(Int,(a,a))]
flatCount xs = do
  let flati = flattenAll xs
  zip (count xs flati) (unflatten flati)

discreteHistogram :: [[(DateTime,DateTime)]]-> [(Int,Int)]
discreteHistogram xs = do
          let counts = flatCount xs
          let possibilities = map fst counts & nubSort
          let aggregated = map (\n-> (n,filter ((n==).fst) counts)) possibilities
          let simplified = map (\(n, identicallyAnnotated)-> (n, map snd identicallyAnnotated)) aggregated
          let measured = map (\(n,intervals)-> (n, map measure' intervals)) simplified
          map (\(n,lengths)-> (n,sum lengths)) measured

nubSort:: Ord a=> [a]-> [a]
nubSort = sort <&> fastnub
    where fastnub(one:other:rest) = if one==other then fastnub(one:rest) else one:fastnub(other:rest)
          fastnub short = short
