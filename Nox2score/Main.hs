{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Control.Applicative (liftA2, some )
import Control.Monad( filterM, when )
import Data.Char
import Data.Function( (&) )
import Data.Functor( (<&>) )
import Data.Foldable( traverse_ )
import Data.List
import System.Directory( getDirectoryContents )
import System.Environment( getArgs )
import System.Exit( die )
import System.FilePath( FilePath, takeFileName, (</>), (<.>) )
import System.IO( Handle, IOMode(ReadMode, WriteMode), hGetContents, hPutStr, hSetEncoding, utf16, utf8, withBinaryFile, withFile )
import Text.ParserCombinators.ReadP( ReadP, char, eof, many, readP_to_S, satisfy, skipMany1, string )

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap
infixl 8 >$

main:: IO ()
main= do
  paths <- getArgs
  if paths `fewerThan` 2
    then mapM_ putStrLn ["Nox2score version 0", "Usage: Nox2score DESTINATION FILE..."]
    else traverse_ (head paths & make) (tail paths)

make:: FilePath-> FilePath-> IO ()
make  destinationFolder inputPath=
  withBinaryFile inputPath ReadMode (convertAndSave destinationFolder)

convertAndSave:: FilePath-> Handle-> IO ()
convertAndSave destinationFolder inputHandle=
     hSetEncoding inputHandle utf16
  >> hGetContents inputHandle >$ convert
  >>= save destinationFolder

save:: FilePath-> (DateTime, String)-> IO ()
save destinationFolder ((DateTime year month day _ _ _), output)= do
  stem <- recordingName year month day
  withFile (destinationFolder </> stem <.> "csv") WriteMode (write output)

write:: String-> Handle-> IO ()
write output handle=
      hSetEncoding handle utf8
  >> hPutStr handle output

isRecordingName:: FilePath-> Bool
isRecordingName= readP_to_S (do
                                     _ <- string "VSN-14-080-0"
                                     skipMany1 (satisfy (/='_'))
                                     _ <- string ".txt"
                                     eof
                                 ) <&> (not.null)

recordingName:: Int-> Int-> Int-> IO FilePath
recordingName year month day = do
  let date = format (DateTime year month day 0 0 0) & take 10
  entries <- getDirectoryContents "../NoxPes2Csv/nadir/BbB/"
  let possibilities = filter isRecordingName entries :: [FilePath]
  candidates <- filterM (startsOn date) (map ("../NoxPes2Csv/nadir/BbB/" ++) possibilities)
  when (tail candidates & (not.null)) . die $ "No more than one recording expected on day " ++ date ++ "!"
  let only = head candidates
  let Just filename = stripPrefix "../NoxPes2Csv/nadir/BbB/" only
  return $! take (length "VSN-14-080-0NN") filename

startsOn:: String-> FilePath-> IO Bool
startsOn date path= do
  withFile path ReadMode (startsWith date)

startsWith:: String-> Handle-> IO Bool
startsWith date handle= do
  contents <- (hGetContents handle)
  return $! take 10 contents == date

-- In: file in Noxturnal CSV format
-- Out: (recording start datetime, file in NumPy CSV format)
convert:: String-> (DateTime, String)
convert= (readP_to_S file :: String-> [([(DateTime, DateTime)], String)])
  >$ last
  >$ fst
  >$ \intervals-> (intervals & head & fst, intervals >>= formatRow)

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

pad :: String-> String
pad number@[_]= '0':number
pad number@[_,_]= number
pad _= error "pad: only one or two digits allowed!"

format:: DateTime-> String
format (DateTime year month day hour minute second)=
         "-" `intercalate` (show year : fmap (show >$ pad) [month, day])
     ++ " "
     ++ ":" `intercalate` fmap (show >$ pad)  [hour, minute, second]
     ++ ".000000"

formatRow:: (DateTime, DateTime)-> String
formatRow (beginning, end)= format beginning ++ "," ++ format end ++ "\n" -- carriage return implicit
