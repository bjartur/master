{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import Control.Applicative( liftA2, some )
import Control.Monad ( forM )
import Data.Char
import Data.List ( subsequences )
import Data.Function( on, (&) )
import System.Environment( getArgs )
import Text.ParserCombinators.ReadP( ReadP, char, eof, get, readP_to_S, satisfy, (<++) )
import qualified Data.Interval     as Interval
import qualified Data.IntervalSet  as IntervalSet
import Data.Interval (Extended(Finite), Boundary(Closed))
import System.FilePath ( takeFileName )
import Plot (renderOverlaps)

import Bjartur.Types
import Bjartur.Records ( intervals, readIntervals )

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 1 >$

combinations :: [a] -> [(a,a)]
combinations = map pair . filter length2 . subsequences
  where
    length2 list = length list == 2
    pair [e1,e2] = (e1,e2)

main:: IO ()
main= do
  paths <- getArgs
  if paths `fewerThan` 2
    then 
      --mapM_ putStrLn ["Overlap version 0", "Usage: overlap ONE OTHER [MORE ...]"]
      do
        scores <- intervals
        forM (combinations scores) $ \(a@(nameLeft, scoresLeft), b@(nameRight, scoresRight)) -> do
          let outPath = "output/" ++ nameLeft ++ "-" ++ nameRight ++ ".svg"
          putStrLn $ "Writing " ++ outPath
          overlaps <- doPair a b
          renderOverlaps outPath [overlaps]
        return ()
    else do
      intervalss <- forM paths $ \path -> do
        let name = takeFileName path
        intervals <- readIntervals path
        pure (name, intervals)
      overlaps <- mapM (uncurry doPair) (combinations intervalss)
      renderOverlaps "overlaps.svg" overlaps
      putStrLn "Wrote overlaps.svg"
  return ()


doPair :: (String, Intervals) -> (String, Intervals)
       -> IO (([Char], [Double], [Char]))
doPair (fName, former) (lName, latter) = do
  let left  = onlyLeft former latter
      right = onlyLeft latter former
      intersection = correlation former latter
  putStrLn $ "left (" ++ fName ++ "):  " ++ show left
  putStrLn $ "right (" ++ lName ++ "): " ++ show right
  putStrLn $ "intersection: " ++ show intersection
  return (fName, [left,intersection,right], lName)

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
