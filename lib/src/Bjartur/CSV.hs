module Bjartur.CSV ( parse ) where

import Bjartur.Types

import Control.Applicative( liftA2, some, many )
import Data.Char ( isDigit, ord )
import Text.ParserCombinators.ReadP( ReadP, char, eof, get, readP_to_S, satisfy, (<++) )
import qualified Data.Interval     as Interval
import qualified Data.IntervalSet  as IntervalSet
import Data.Interval (Extended(Finite), Boundary(Closed))

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 1 >$

parse:: String-> Intervals
parse= readP_to_S file
    >$ last >$ fst

file:: ReadP Intervals
file = many row <* eof >$ IntervalSet.fromList

row:: ReadP Interval
row= do
  start <- dateTime
  expect ','
  end <- dateTime
  expect '\n' -- native line separator has already been translated to \n
  return $ period start end

period :: Ord r=> r-> r-> Interval.Interval r
period start end=
  Interval.interval
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
