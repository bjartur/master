{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Control.Applicative (liftA2, some, (<|>) )
import Control.Monad( guard, filterM, when )
import Data.Char
import Data.Function( (&) )
import Data.Functor( (<&>), ($>) )
import Data.Foldable( traverse_ )
import Data.List
import System.Directory( getDirectoryContents )
import System.Environment( getArgs )
import System.Exit( die )
import System.FilePath( takeFileName, (</>), (<.>) )
import System.IO( Handle, IOMode(ReadMode, WriteMode), hGetContents, hPutStr, hSetEncoding, utf16, utf8, withBinaryFile, withFile )
import Text.ParserCombinators.ReadP( ReadP, char, eof, get, look, many, manyTill, optional, readP_to_S, satisfy, string, (<++) )

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
make  destinationFolder inputPath= do
  let date = take 8 (takeFileName inputPath)
  withBinaryFile inputPath ReadMode (convertAndSave destinationFolder date)

convertAndSave:: FilePath-> String-> Handle-> IO ()
convertAndSave destinationFolder date inputHandle=
     hSetEncoding inputHandle utf16
  >> hGetContents inputHandle >$ convert
  >>= save destinationFolder date

save:: FilePath-> String-> String-> IO ()
save destinationFolder date output= do
  stem <- recordingName date
  withFile (destinationFolder </> stem <.> "csv") WriteMode (write output)

write:: String-> Handle-> IO ()
write output handle=
      hSetEncoding handle utf8
  >> hPutStr handle output

isRecordingName:: FilePath-> Bool
isRecordingName= readP_to_S (do
                                     string "VSN-14-080-0"
                                     get
                                     get
                                     optional (char '/' <|> char '\\')
                                     eof
                                 ) <&> (not.null)

recordingFolder:: FilePath
recordingFolder = "../NoxPes2Csv/VSN-14-080/"

recordings:: IO [FilePath]
recordings= do
      getDirectoryContents recordingFolder
  <&> filter isRecordingName
  <&> map (recordingFolder </>)

recordingName:: String-> IO FilePath
recordingName date = do

  candidates <- recordings >>= filterM (startsOn date)
  when (null candidates) . die $ "Could not find any recording from day " ++ date ++ "!"
  when (tail candidates & (not.null)) . die $ "No more than one recording expected on day " ++ date ++ "!"
  let only = head candidates

  putStrLn $ "Recording " ++ takeFileName only ++ " corresponds to manual scoring from " ++ date ++ "."
  return $! takeFileName only

startsOn:: String-> FilePath-> IO Bool
startsOn date path= do
  withFile (path </> "EventLog.txt") ReadMode (startsWith date)

data EventCode = RecordingStarted | Other deriving Eq
data Event = Event{eventCode::EventCode, eventDate::String, eventMessage::String}

parseEvent:: String-> Event
parseEvent= parse $ do
  code <- liftA2 (+) (fmap (100*) couple) couple
  expect ';'
  date <- manyTill get (char 'T')
  manyTill get (char ';') $> ()
  message <- look
  return Event{eventCode=if code == 114 then RecordingStarted else Other, eventDate=date, eventMessage=message}

startsWith:: String-> Handle-> IO Bool
startsWith date handle= do
  events <- hGetContents handle <&> lines <&> map parseEvent
  let startingDates = do
        Event code candiDate _ <- events
        guard $ code == RecordingStarted
        return candiDate
  return $! date == head startingDates

-- In: file in Noxturnal CSV format
-- Out: (recording start datetime, file in NumPy CSV format)
convert:: String-> String
convert= (readP_to_S file :: String-> [([(DateTime, DateTime)], String)])
  >$ last
  >$ fst
  >$ (>>= formatRow)

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

parse:: ReadP a-> String-> a
parse parser= readP_to_S parser >$ last >$ fst

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
  expect '/'
  month <- couple
  expect '/'
  year <- liftA2 (+) (fmap (100*) couple) couple
  expect ' '
  hour <- couple
  expect ':'
  minute <- couple
  expect ':'
  second <- couple
  expect ','
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

expect:: Char-> ReadP ()
expect character= char character <++ (get >>= expected (show character)) $> ()

expected:: Show a=> String-> a-> ReadP bottom
expected expectation reality= error ("\n\tExpected " ++ expectation ++ " but got " ++ show reality ++ "!\n")

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
