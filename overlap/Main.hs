{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import Control.Applicative( liftA2, some )
import Data.Char
import Data.Function( on, (&) )
import System.Environment( getArgs )
import Text.ParserCombinators.ReadP( ReadP, char, eof, get, readP_to_S, satisfy, (<++) )
import qualified Data.Interval     as Interval
import qualified Data.IntervalSet  as IntervalSet
import Data.Interval (Extended(Finite), Boundary(Closed))

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 1 >$

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

main:: IO ()
main= do
  paths <- getArgs
  if paths `fewerThan` 2
    then mapM_ putStrLn ["Overlap version 0", "Usage: overlap ONE OTHER"]
    else
        (traverse readFile paths ::IO [String])
    >>= (\[former,latter]-> correlation (parse former :: Intervals) (parse latter :: Intervals)
                          & print)

correlation:: Intervals -> Intervals -> Double
correlation one other= do
  let dividedBy = (/) `on` fromIntegral
  let total = measures (union one other)
  if total == 0
  then undefined
  else overlaps one other `dividedBy` total

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

parse:: String-> Intervals
parse= readP_to_S file
    >$ last >$ fst

file:: ReadP Intervals
file = some row <* eof >$ IntervalSet.fromList

row:: ReadP Interval
row= do
  start <- dateTime
  expect ','
  end <- dateTime
  expect '\n' -- native line separator has already been translated to \n
  return $ Interval.interval
    (Finite start, Closed)
    (Finite end,   Closed)

dateTime:: ReadP DateTime
dateTime= do
  year <- liftA2 (+) (fmap (100*) couple) couple
  expect '-'
  month <- couple
  expect '-'
  day <- couple
  expect ' '
  hour <- couple
  expect ':'
  minute <- couple
  expect ':'
  second <- couple
  expect '.'
  centisecond <- couple
  couple
  couple
  return $ DateTime year month day hour minute (if centisecond >= 50 then second + 1 else second)

couple:: ReadP Int
couple=
  (do
    tens <- digit
    singles <- digit
    return (tens*10 + singles)
  ) <++ (sequence[get,get] >>= expected "a couple of digits")

digit:: ReadP Int
digit= satisfy isDigit
  >$ ord
  >$ \ascii-> ascii-48

expect:: Char-> ReadP Char
expect character= char character <++ (get >>= expected (show character))

expected:: Show a=> String-> a-> ReadP bottom
expected expectation reality= error ("\n\tExpected " ++ expectation ++ " but got " ++ show reality ++ "!\n")

fewerThan :: [a]-> Int-> Bool
elements `fewerThan` n = null $ drop (n-1) elements
