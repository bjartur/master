{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import Control.Applicative( liftA2, some )
import Data.Char
import Data.Function( (&), on )
import System.Environment( getArgs )
import Text.ParserCombinators.ReadP( ReadP, char, eof, readP_to_S, satisfy, string )

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 8 >$

type Interval= (DateTime, DateTime)
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
    else traverse readFile paths >>= mapM_ (readP_to_S file >$ print)

correlation:: [Interval]-> [Interval]-> Double
correlation one other= do
  let dividedBy = (/) `on` fromIntegral
  let total= map measure(union one other) & sum
  if total == 0
  then 1.0
  else overlaps one other `dividedBy` total

-- @union ones others@ calculates a union of the given ascending lists of intervals, ordered by their start time.
-- If each input list contains only disjoint intervals, the same will hold for the result.
-- If the input is represented by exclusive intervals, so will the result be, and vice versa.
union:: [Interval]-> [Interval]-> [Interval]
union [] intervals= intervals
union intervals []= intervals
union ((start,stop):formers) ((beginning,end):latters)=
  if      end < start      then (beginning,end) : union formers ((start,stop):latters)
  else if stop < beginning then (start,stop) : union latters ((beginning,end):formers)
  else                          (min start beginning, max stop end) : union formers latters

-- @overlaps ones others@ measures the intersection of the two given countable unions of intervals,
-- assuming each argument is an ascending list of disjoint intervals.
overlaps:: [Interval]-> [Interval]-> Int
overlaps [] _= 0
overlaps _ []= 0
overlaps ((beginning,end):formers)((start,stop):latters)=
  if      end < start      then overlaps ((start,stop):latters) formers
  else if stop < beginning then overlaps latters ((beginning,end):formers)
  else overlap (beginning,end)(start,stop)
       + if end < stop
         then overlaps formers ((end,stop):latters)
         else overlaps ((stop,end):formers)latters

measure:: (DateTime, DateTime)-> Int
measure(from, to)= let
  seconds accessor unit= unit * (accessor to - accessor from)
  increments= [1, 60, 60, 24, 365]
  accessors = [second, minute, hour, day, year]
  units= [1..length increments] >$ flip take increments >$ product
  in
  zipWith seconds accessors units & sum

overlap:: Interval-> Interval-> Int
overlap (from, to) (start, end)= measure(max from start, min to end)

file:: ReadP [ (DateTime, DateTime) ]
file= some row <* eof

row:: ReadP (DateTime, DateTime)
row= do
  start <- dateTime
  char ','
  end <- dateTime
  string "\r\n"
  return (start, end)

dateTime:: ReadP DateTime
dateTime= do
  year <- liftA2 (+) (fmap (100*) couple) couple
  char '-'
  month <- couple
  char '-'
  day <- couple
  char ' '
  hour <- couple
  char ':'
  minute <- couple
  char ':'
  second <- couple
  char '.'
  centisecond <- couple
  couple
  couple
  return $ DateTime year month day hour minute (if centisecond >= 50 then second + 1 else second)

couple:: ReadP Int
couple= do
  tens <- digit
  singles <- digit
  return (tens*10 + singles)

digit:: ReadP Int
digit= satisfy isDigit
  >$ ord
  >$ \ascii-> ascii-48

fewerThan :: [a]-> Int-> Bool
elements `fewerThan` n = null $ drop (n-1) elements
