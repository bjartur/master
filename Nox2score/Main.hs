{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Control.Applicative (liftA2, some )
import Control.Monad( forM_, join )
import Data.Char
import Data.List
import System.Environment( getArgs )
import System.FilePath( takeFileName, (</>) )
import System.IO( IOMode(ReadMode, WriteMode), hGetContents, hPutStr, hSetEncoding, utf16, utf8, withBinaryFile, withFile )
import Text.ParserCombinators.ReadP( ReadP, char, eof, many, optional, readP_to_S, satisfy, string )

x & f = f x
(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 8 >$

(>>$):: (Functor outer, Functor inner)=> outer( inner input )-> (input-> output)-> outer ( inner output )
input >>$ function = input >$ fmap function
infixl 8 >>$

main:: IO ()
main= (do
  paths <- getArgs
  if paths `fewerThan` 2
    then mapM_ putStrLn ["Nox2score version 0", "Usage: Nox2score DESTINATION FILE..."]
    else let destination = head paths in
      forM_ (tail paths) $ \path->
        withBinaryFile path ReadMode $ \inputHandle->
         (hSetEncoding inputHandle utf16
       >> hGetContents inputHandle
          >$ convert
          >>= withFile (destination </> takeFileName path) WriteMode . \output-> \outputHandle->
                hSetEncoding outputHandle utf8
            >> hPutStr outputHandle output)
  )

convert:: String-> String
convert= (readP_to_S file :: String-> [([(DateTime, DateTime)], String)])
  >$ last
  >$ fst
  >$ (formatRow =<<)

file:: ReadP [ (DateTime, DateTime) ]
file= do
  string "Start Time,End Time,Sleep,\r\n"
  string "[],[],[],\r\n"
  body <- some row
  eof
  return body

row:: ReadP (DateTime, DateTime)
row= do
  start <- dateTime
  end <- dateTime
  many $ satisfy (liftA2 (&&) (/='\r') (/='\n'))
  string "\r\n"
  return (start, end)

data DateTime= DateTime
 Int -- year
 Int -- month
 Int -- day
 Int -- hour
 Int -- minute
 Int -- second
 deriving (Show, Eq)


dateTime:: ReadP DateTime
dateTime= do
  day <- couple
  char '/'
  month <- couple
  char '/'
  year <- liftA2 (+) (fmap (100*) couple) couple
  char ' '
  hour <- couple
  char ':'
  minute <- couple
  char ':'
  second <- couple
  char ','
  return $ DateTime year month day hour minute second

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

format:: DateTime-> String
format (DateTime year month day hour minute second)=
         "-" `intercalate` fmap show [year, month, day]
     ++ " "
     ++ ":" `intercalate` fmap show [hour, minute, second]
     ++ ".000000"

formatRow:: (DateTime, DateTime)-> String
formatRow (beginning, end)= format beginning ++ "," ++ format end
