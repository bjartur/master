{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import Control.Applicative ( liftA2 )
import Control.Monad ( forM, forM_ )
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

import Bjartur.Time
import Bjartur.Records ( autoscoredIntervals, intervals, readIntervals, tst )

type Number = Ratio Int

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 1 >$

combinations:: [a] -> [(a,a)]
combinations list= [(x,y) | (x:ys) <- tails list, y <- ys]

when:: Monoid m=> Bool-> m-> m
when True m= m
when False _= mempty

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

  let (detailedSummaries, stats) = map (uncurry statistics) (combinations scores) & unzip ::([String], [(Ratio Int, String, (Number, Number, Number), String, Ratio Int)])
  mapM_ putStr detailedSummaries
  mapM_ (putStr . uncurry absoluteStatistics) (combinations scores)

-- Bars, side-by-side
  forM_ stats $ \(_, leftName, percentages, rightName, _) -> (do
    let outPath = "output/" ++ leftName ++ "-" ++ rightName ++ ".svg"
    putStrLn $ "Writing " ++ outPath
    renderOverlaps outPath leftName percentages rightName)

  let calculate (label,statistic)= (label, stats >$ (\(l, leftName, (left,intersectionOverUnion,right), rightName, r)-> (l, leftName, statistic left intersectionOverUnion right, rightName, r)))
  let jaccards _ intersectionOverUnion _= intersectionOverUnion
  let report frameProportions = putStr . tabulate frameProportions . calculate
  report False ("Jaccard", jaccards)
  report True ("Overlap coefficient", coefficient)

indent:: Int-> [String]-> String
indent indent= intercalate "\t" >$ (replicate indent '\t' ++)

tabulate:: Bool-> (String, [(Ratio Int, String, Number, String, Ratio Int)])-> String
tabulate frameProportions (title, coefficients)=
  "\n" ++ map toUpper title ++ "\n" ++ let
    rowLengths = [14-1, 14-2 .. 1]
    keepsAndSkips = if length coefficients /= sum rowLengths
      then error("Miscalculation: expected " ++ show (sum rowLengths) ++ " coefficients, but found " ++ show (length coefficients) ++ "!")
      else zip rowLengths (inits rowLengths) & map (fmap sum) ::[(Int, Int)]
    listsOfTriplets = keepsAndSkips >$ (\(keep, skip)-> drop skip coefficients & take keep) ::[[(Ratio Int, String, Number, String, Ratio Int)]]
    reversed = map reverse listsOfTriplets :: [[(Ratio Int, String, Number, String, Ratio Int)]]
    top = head reversed >$ (\(_,_,_,_,proportion)-> formatPercentage proportion) & indent 2 ::String
    header = head reversed >$ (\(_,_,_,rightName,_)-> rightName) & indent (if frameProportions then 2 else 1) ::String
    table = reversed >$ liftA2 (,)
      (head >$ (\(proportion,leftName,_,_,_)-> when frameProportions (formatPercentage proportion ++ "\t") ++ leftName))
      (map (\(_,_,statistic,_,_)-> statistic))
        ::[(String, [Number])]
    rows = table >$ fmap (map formatPercentage >$ intercalate "\t") ::[(String, String)]
    indented = map (\(leftName, row) -> (leftName ++ "\t" ++ row)) rows
    in when frameProportions (top ++ "\n") ++ header ++ "\n" ++ intercalate "\n" indented ++ "\n"

-- combined sleep time of all polysomnograms in seconds
totalSleepTime:: Int
totalSleepTime= round(60 * 60 * tst "total")

statistics:: (String, Intervals)-> (String, Intervals)-> (String, (Ratio Int, String, (Number,Number,Number), String, Ratio Int) )
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
      (measures left % totalSleepTime, leftName, (former,intersectionOverUnion,latter), rightName, measures right % totalSleepTime)
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

measures :: Intervals -> Int
measures is = map measure (IntervalSet.toList is) & sum

meanIntervalLength :: Intervals -> Number
meanIntervalLength = IntervalSet.toList
  >$ map measure
  >$ (\list-> sum list `dividedBy` length list)

fewerThan :: [a]-> Int-> Bool
elements `fewerThan` n = null $ drop (n-1) elements
