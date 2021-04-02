{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import Control.Applicative ( liftA2 )
import Control.Monad ( forM, forM_, when )
import Data.Char( toUpper )
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
import Bjartur.Records ( autoscoredIntervals, intervals, readIntervals, tst )

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
  putStr "Manual and automatic scoring: "
  putStrLn (scores & summary)
  putStr "Programmatic scoring: "
  autoscoredIntervals >$ summary >>= putStrLn
  when leave1out $ forM_ scores $ \score -> (do
    let (name,_) = score
    let dropped = filter (/= score) scores
    putStrLn $ "without classifier " ++ name ++ " (whose mean interval length is " ++ (score & snd & meanIntervalLength & show) ++ "):\t"
              ++ summary dropped)

  putStrLn ""

  let (detailedSummaries, stats) = map (uncurry statistics) (combinations scores) & unzip :: ([String], [(String, (Number, Number, Number), String)])
  mapM_ putStr detailedSummaries
  mapM_ (putStr . uncurry absoluteStatistics) (combinations scores)

-- Bars, side-by-side
  forM_ stats $ \(leftName, percentages, rightName) -> (do
    let outPath = "output/" ++ leftName ++ "-" ++ rightName ++ ".svg"
    putStrLn $ "Writing " ++ outPath
    renderOverlaps outPath leftName percentages rightName)

  let calculate (label,statistic)= (label, stats >$ (\(leftName, (left,intersectionOverUnion,right), rightName)-> (leftName, statistic left intersectionOverUnion right, rightName)))
  let jaccards _ intersectionOverUnion _= intersectionOverUnion
  mapM_ (putStr . tabulate . calculate) [("Jaccard", jaccards), ("Overlap coefficient", coefficient)]

tabulate:: (String, [(String, Number, String)])-> String
tabulate (title, coefficients)=
  "\n" ++ map toUpper title ++ "\n" ++ let
    rowLengths = [14-1, 14-2 .. 1]
    keepsAndSkips = if length coefficients /= sum rowLengths
      then error("Miscalculation: expected " ++ show (sum rowLengths) ++ " coefficients, but found " ++ show (length coefficients) ++ "!")
      else zip rowLengths (inits rowLengths) & map (fmap sum) :: [(Int, Int)]
    listsOfTriplets = keepsAndSkips >$ (\(keep, skip)-> drop skip coefficients & take keep) :: [[(String, Number, String)]]
    reversed = map reverse listsOfTriplets :: [[(String, Number, String)]]
    header = head reversed >$ (\(_,_,rightName) -> rightName) & ("":) & intercalate "\t" :: String
    table = reversed >$ liftA2 (,) (head >$ (\(leftName,_,_) -> leftName)) (map (\(_,statistic,_)-> statistic)) :: [(String, [Number])]
    rows = table >$ fmap (map formatPercentage >$ intercalate "\t") :: [(String, String)]
    indented = map (\(leftName, row) -> (leftName ++ "\t" ++ row)) rows
    in header ++ "\n" ++ intercalate "\n" indented ++ "\n"

-- combined sleep time of all polysomnograms in seconds
totalSleepTime:: Int
totalSleepTime= round(60 * 60 * tst "total")

statistics:: (String, Intervals)-> (String, Intervals)-> (String, (String, (Number,Number,Number), String) )
statistics (leftName, left) (rightName, right) =
  let former = onlyLeft left right
      intersectionOverUnion = jaccard left right
      latter = 1 - former - intersectionOverUnion
  in seq(if latter==onlyLeft right left then () else error $ "latter - onlyLeft right left = " ++ show (latter - onlyLeft right left))
  (
      "left (" ++ leftName ++ "):  " ++ show former ++ "\t" ++
      "jaccard: " ++ show intersectionOverUnion ++ "\t" ++
      "right (" ++ rightName ++ "): " ++ show latter ++ "\t" ++
      "overlap coefficient: " ++ show (coefficient former intersectionOverUnion latter) ++
      "\n"
   ,
      (leftName, (former,intersectionOverUnion,latter), rightName)
  )

absoluteStatistics:: (String, Intervals)-> (String, Intervals)-> String
absoluteStatistics (leftName, left) (rightName, right)=
  let both = measures(IntervalSet.intersection left right)
      neither = totalSleepTime - measures(IntervalSet.union left right)
      trueLeftFalseRight = measures(left `IntervalSet.difference` right)
      trueRightFalseLeft = measures(right `IntervalSet.difference` left)
  in seq (if totalSleepTime == both + trueLeftFalseRight + trueRightFalseLeft + neither then () else undefined)
    leftName ++ "\t( "
          ++ show trueLeftFalseRight
          ++ " ( " ++ show both ++ " ) "
          ++ show trueRightFalseLeft
          ++ " )\t" ++ rightName
          ++ "\t[" ++ show neither ++ "]"
          ++ "\n"

-- Overlap coefficient
-- https://en.wikipedia.org/wiki/Overlap_coefficient
coefficient :: Number-> Number-> Number-> Number
coefficient left intersection right= intersection / (intersection + min left right)

-- Overlap coefficient for multiple sets
summary :: [(label, Intervals)]-> String
summary intervals =
  let sets = map snd intervals
      intersectionInSeconds = measures $ IntervalSet.intersections sets
      leastSensitiveMethodInSeconds = foldl1 min $ map measures sets
      statistic = fromIntegral intersectionInSeconds / fromIntegral leastSensitiveMethodInSeconds :: Number
  in
    "intersection: " ++ show intersectionInSeconds ++ " seconds\t" ++ "overlap coefficient: " ++ show statistic


-- on measures (/) (union one other) (intersection one other)
jaccard :: Intervals -> Intervals -> Number
jaccard one other = do
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
