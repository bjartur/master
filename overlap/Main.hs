{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import Control.Monad ( forM, forM_ )
import Data.List ( tails )
import Data.Function( on, (&) )
import System.Environment( getArgs )
import qualified Data.Interval     as Interval
import qualified Data.IntervalSet  as IntervalSet
import Data.Interval (Extended(Finite))
import System.FilePath ( takeFileName )
import Plot (renderOverlaps)

import Bjartur.Types
import Bjartur.Records ( intervals, readIntervals )

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 1 >$

combinations:: [a] -> [(a,a)]
combinations list= [(x,y) | (x:ys) <- tails list, y <- ys]

main:: IO ()
main= do
  paths <- getArgs
  if paths `fewerThan` 2
    then do
      mapM_ putStrLn ["Overlap version 0", "Usage: overlap ONE OTHER [MORE ...]"]
      scores <- intervals
      -- Calculate the coefficient of all classifier sets
      putStrLn $ "coefficient of all classifiers: "
                ++ show (coefficient' scores)
      forM_ scores $ \score -> do
        let (name,_) = score
        let dropped = filter (/= score) scores
        putStrLn $ "coefficient without classifier " ++ name ++ ": "
                  ++ show (coefficient' dropped)
      --
      forM_ (combinations scores) $ \(a, b) -> do
        (leftName, stats, rightName) <- statistics a b
        let outPath = "output/" ++ leftName ++ "-" ++ rightName ++ ".svg"
        putStrLn $ "Writing " ++ outPath
        renderOverlaps outPath [(leftName, stats, rightName)]
    else do
      intervalss <- forM paths $ \path -> do
        let name = takeFileName path
        intervals <- readIntervals path
        pure (name, intervals)
      overlaps <- mapM (uncurry statistics) (combinations intervalss)
      renderOverlaps "overlaps.svg" overlaps
      putStrLn "Wrote overlaps.svg"


statistics :: (String, Intervals) -> (String, Intervals)
       -> IO (([Char], (Double,Double,Double), [Char]))
statistics (fName, former) (lName, latter) = do
  let left  = onlyLeft former latter
      right = onlyLeft latter former
      intersection = correlation former latter
  putStrLn $ "left (" ++ fName ++ "):  " ++ show left
  putStrLn $ "right (" ++ lName ++ "): " ++ show right
  putStrLn $ "intersection: " ++ show intersection
  putStrLn $ "coefficient: " ++ show (coefficient left intersection right)
  return (fName, (left,intersection,right), lName)

-- Overlap coefficient
-- https://en.wikipedia.org/wiki/Overlap_coefficient
coefficient :: (Fractional n, Ord n) => n -> n -> n -> n
coefficient l o r = o / (o + min l r)

-- Overlap coefficient for multiple sets
coefficient' :: [(label, Intervals)]-> Double
coefficient' intervals =
  let sets = map snd intervals
      counter = measures $ IntervalSet.intersections sets
      denomin = foldl1 min $ map measures sets
  in (fromIntegral counter / fromIntegral denomin)

-- Ratio of measures that intersect on both sides over total
correlation:: Intervals -> Intervals -> Double
correlation one other= do
  let dividedBy = (/) `on` fromIntegral
  let total = measures (union one other)
  if total == 0
  then undefined
  else overlaps one other `dividedBy` total

-- Ratio of measures on the left-hand side
onlyLeft :: Intervals -> Intervals -> Double
onlyLeft one other = (one `IntervalSet.difference` intersect & measures) `dividedBy` (measures $ union one other)
  where intersect = IntervalSet.intersection one other
        dividedBy = (/) `on` fromIntegral

-- @union ones others@ calculates a union of the given ascending lists of intervals, ordered by their start time.
-- If each input list contains only disjoint intervals, the same will hold for the result.
-- If the input is represented by exclusive intervals, so will the result be, and vice versa.
union :: Intervals -> Intervals -> Intervals
union = IntervalSet.union

-- @overlaps ones others@ measures the intersection of the two given countable unions of intervals,
-- assuming each argument is an ascending list of disjoint intervals.
overlaps:: Intervals -> Intervals -> Int
overlaps a b = measures $ IntervalSet.intersection a b

measure :: Interval -> Int
measure i = let (Finite lower) = Interval.lowerBound i
                (Finite upper) = Interval.upperBound i
            in measure' (lower, upper)

measures :: Intervals -> Int
measures is = map measure (IntervalSet.toList is) & sum

measure' :: (DateTime, DateTime)-> Int
measure' (from, to)= let
  seconds accessor unit= unit * (accessor to - accessor from)
  increments= [1, 60, 60, 24, 365]
  accessors = [second, minute, hour, day, year]
  units= [1..length increments] >$ flip take increments >$ product
  in
  zipWith seconds accessors units & sum

fewerThan :: [a]-> Int-> Bool
elements `fewerThan` n = null $ drop (n-1) elements
