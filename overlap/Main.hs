{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
module Main where

import Control.Applicative( liftA2, some )
import Data.Char
import Data.Function( (&) )
import System.Environment( getArgs )
import Text.ParserCombinators.ReadP( ReadP, char, eof, readP_to_S, satisfy, string )

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 8 >$

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

diff:: a
diff = undefined

intersection:: (DateTime, DateTime)-> (DateTime, DateTime)-> (DateTime, DateTime)
intersection (from, to) (start, end)= (max from start, min to end)

interval:: (DateTime, DateTime)-> Int
interval (from, to)= let
  seconds accessor unit= unit * (accessor to - accessor from)
  increments= [1, 60, 60, 24, 365]
  accessors = [second, minute, hour, day, year]
  units= [1..length increments] >$ flip take increments >$ product
  in
  zipWith seconds accessors units & sum

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
