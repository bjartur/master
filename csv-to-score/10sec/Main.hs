module Main where

import Bjartur.CSV( dateTime, expect )
import Bjartur.Time( DateTime, measure' )
import Data.Functor( (<&>) )
import Text.ParserCombinators.ReadP( ReadP, readP_to_S )

parse readP= readP_to_S readP
    <&> last <&> fst

line:: ReadP (DateTime, DateTime)
line= do
  beginning <- dateTime
  expect ','
  end <- dateTime
  return (beginning, end)

longerThan10Seconds:: String-> Bool
longerThan10Seconds=
      parse line
  <&> measure'
  <&> (> 10)

main= interact (lines <&> filter longerThan10Seconds <&> unlines)
