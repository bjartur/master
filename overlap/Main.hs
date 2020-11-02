{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import Control.Applicative ( liftA2 )
import Control.Monad ( forM, forM_, unless, when )
import Data.Function( (&), on )
import Data.List( inits, intercalate, tails )
import Data.Ratio( (%), Ratio )
import System.Environment( getArgs )
import qualified Data.Interval     as Interval
import qualified Data.IntervalSet  as IntervalSet
import Data.Interval (Extended(Finite))
import System.FilePath ( takeFileName )
import Plot (formatPercentage, renderOverlaps)

import Bjartur.Types
import Bjartur.Records ( intervals, readIntervals )

type Number = Ratio Int

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 1 >$

combinations:: [a] -> [(a,a)]
combinations list= [(x,y) | (x:ys) <- tails list, y <- ys]

main:: IO ()
main= do
  paths <- getArgs
  let leave1out = length paths == 1
  scores <- if length paths < 2 then intervals else
      forM paths $ \path -> (do
        let name = takeFileName path
        intervals <- readIntervals path
        pure (name, intervals))

-- Calculate the coefficient of all classifier sets
  putStrLn (scores & summary)
  when leave1out $ forM_ scores $ \score -> (do
    let (name,_) = score
    let dropped = filter (/= score) scores
    putStrLn $ "without classifier " ++ name ++ " (whose mean interval length is " ++ (score & snd & meanIntervalLength & show) ++ "):\t"
              ++ summary dropped)

  putStrLn ""

  stats <- traverse (uncurry statistics) (combinations scores)

-- Bars, side-by-side
  forM_ stats $ \(leftName, percentages, rightName) -> (do
    let outPath = "output/" ++ leftName ++ "-" ++ rightName ++ ".svg"
    putStrLn $ "Writing " ++ outPath
    renderOverlaps outPath leftName percentages rightName)

  putStrLn ""

  let rowLengths = [14-1, 14-2 .. 1]
  unless (length stats == sum rowLengths) $ error $ "Miscalculation: expected " ++ show (sum rowLengths) ++ " overlap coefficients, but found " ++ show (length stats) ++ "!"
  let keepsAndSkips = zip rowLengths (inits rowLengths) & map (fmap sum) :: [(Int, Int)]
  let listsOfTriplets = keepsAndSkips >$ (\(keep, skip)-> drop skip stats & take keep) :: [[(String, (Number, Number, Number), String)]]
  let reversed = map reverse listsOfTriplets
  putStrLn $ head reversed >$ (\(_,_,rightName) -> rightName) & ("":) & intercalate "\t"
  let table = reversed >$ liftA2 (,) (head >$ (\(leftName,_,_) -> leftName)) (map (\(_,(left,jaccard,right),_)-> jaccard / (jaccard + min left right))) :: [(String, [Number])]
  let rows = table >$ fmap (map formatPercentage >$ intercalate "\t") :: [(String, String)]
  let indented = zipWith (\n (leftName, row) -> (leftName ++ "\t" ++ row)) [1..] rows
  mapM_ putStrLn $ indented


statistics :: (String, Intervals) -> (String, Intervals)
       -> IO (([Char], (Number,Number,Number), [Char]))
statistics (fName, former) (lName, latter) = do
  let left  = onlyLeft former latter
      right = onlyLeft latter former
      intersection = correlation former latter
  putStrLn $ "left (" ++ fName ++ "):  " ++ show left
  putStrLn $ "right (" ++ lName ++ "): " ++ show right
  putStrLn $ "intersection: " ++ show intersection
  putStrLn $ "coefficient: " ++ show (coefficient former latter)
  return (fName, (left,intersection,right), lName)

-- Overlap coefficient
-- https://en.wikipedia.org/wiki/Overlap_coefficient
coefficient :: Intervals -> Intervals -> Number
coefficient former latter = measures (IntervalSet.intersection former latter) `dividedBy` on min measures former latter

-- Overlap coefficient for multiple sets
summary :: [(label, Intervals)]-> String
summary intervals =
  let sets = map snd intervals
      intersectionInSeconds = measures $ IntervalSet.intersections sets
      leastSensitiveMethodInSeconds = foldl1 min $ map measures sets
      statistic = fromIntegral intersectionInSeconds / fromIntegral leastSensitiveMethodInSeconds :: Number
  in
    "intersection: " ++ show intersectionInSeconds ++ " seconds\t" ++ "overlap coefficient: " ++ show statistic

-- Ratio of measures that intersect on both sides over total
correlation:: Intervals -> Intervals -> Number
correlation one other= do
  let total = measures (union one other)
  if total == 0
  then undefined
  else overlaps one other `dividedBy` total

-- Ratio of measures on the left-hand side
onlyLeft :: Intervals -> Intervals -> Number
onlyLeft one other = (one `IntervalSet.difference` other & measures) `dividedBy` (measures $ union one other)

dividedBy :: Int -> Int -> Number
dividedBy = (/) `on` (%1)
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

meanIntervalLength :: Intervals -> Number
meanIntervalLength = IntervalSet.toList
  >$ map measure
  >$ (\list-> sum list `dividedBy` length list)

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
