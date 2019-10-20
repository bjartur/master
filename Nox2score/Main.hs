import Control.Applicative (liftA2, some)
import Control.Monad( forM_)
import Data.Char
import Data.List
import System.Environment (getArgs)
import System.FilePath (takeFileName, (</>))
import Text.ParserCombinators.ReadP( ReadP, char, eof, readP_to_S, satisfy, string,  )

(>$):: Functor functor=> functor before-> (before-> after)-> functor after
(>$)= flip fmap

main:: IO ()
main= do
  args <- getArgs
  if args `fewerThan` 2
    then mapM_ putStrLn ["Nox2score version 1", "Usage: Nox2score DESTINATION FILE..."]
    else let destination = head args in
      forM_ (tail args) $ \arg-> (readFile arg >$ convert) >>= writeFile (destination </> takeFileName arg)

convert:: String-> String
convert= (readP_to_S file :: String-> [([(DateTime, DateTime)], String)])
  >$ last
  >$ fst
  >$ (=<<) formatRow

file:: ReadP [ (DateTime, DateTime) ]
file= do
  string "Start Time,End Time,Sleep,\r\n"
  string "[],[],[],\r\n"
  body <- some row
  eof
  return body

row:: ReadP (DateTime, DateTime)
row= do
  start <- date
  end <- date
  some $ satisfy (liftA2 (&&) (/='\r') (/='\n'))
  string "\r\n"
  return (start, end)

data DateTime = DateTime
 Int -- year
 Int -- month
 Int -- day
 Int -- hour
 Int -- minute
 Int -- second

date:: ReadP DateTime
date= do
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
