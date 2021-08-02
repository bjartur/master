module Bjartur.CSV where

import Bjartur.Time

import Control.Applicative( liftA2, some, many )
import Data.Char( isDigit, ord )
import Data.Functor( (<&>) )
import qualified Data.IntervalSet as IntervalSet
import Text.ParserCombinators.ReadP( ReadP, char, eof, get, readP_to_S, satisfy, (<++) )

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 1 >$

parse:: String-> Events
parse= readP_to_S file
    >$ last >$ fst

file:: ReadP Events
file = many row <* eof <&> IntervalSet.fromList

row:: ReadP Event
row= do
  start <- dateTime
  expect ','
  end <- dateTime
  expect '\n' -- native line separator has already been translated to \n
  return $ period start end

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
