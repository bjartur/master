module Main where
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.List (isPrefixOf, stripPrefix)
import Input (CSV, Count, Index, abrupt, belowBaseline, timestampsOfDeclineBeginning, timestampsOfDeclineEnd, (>$), (>>$))
import System.Environment (getArgs)
import System.FilePath (takeFileName, (</>))

spliceRow :: String-> String-> String
spliceRow left right = left ++ (',':right)

spliceColumns :: [String]-> [String]-> [String]
spliceColumns = zipWith spliceRow

scores :: [[Double]-> (Index,Count)-> Double-> Bool]-> Int-> CSV-> [String]
scores = (liftA2.liftA2.liftA2) spliceColumns timestampsOfDeclineBeginning timestampsOfDeclineEnd

scoring :: [[Double]-> (Index,Count)-> Double-> Bool]-> Int-> CSV-> String
scoring =
           scores
  >>$ fmap unlines

fewerThan :: [a]-> Int-> Bool
elements `fewerThan` n = null $ drop (n-1) elements

main :: IO ()
main = do
  args <- getArgs
  let options = takeWhile ("--" `isPrefixOf`) args & takeWhile ("--" /=)
  let parseFlags flags = case flags of {
    ("--unabrupt":_) -> [];
    ("--reversal":_) -> [abrupt];
    (('-':'n':_):rest) -> parseFlags rest;
    _ -> [belowBaseline,abrupt];
  }
  let criteria = parseFlags options
  let Just positionals = stripPrefix options args
  let paths = (if head positionals == "--" then tail else id) positionals
  if paths `fewerThan` 2 || length options > 2
  then mapM_ putStrLn ["csv2score version 1"
                      , "Usage: csv2score [--unabrupt|--reversal|--baseline] [-nN] [--] DESTINATION FILE..."
                      , "If no method is specified, baseline is used by default."
                      , "Reversal drops the requirement that every nadir be under baseline."
                      , "Unabrupt additionally drops the requirement that a crescendo be followed by an nadir above baseline."
                      , "The only legal value for  N is 0 which represents varying the the minimum number of increases in negative pressure from 2 to 5."]
  else forM_ [2..5] $ \n-> score (criteria) (tail paths) n (head paths </> show n)

score:: [[Double]-> (Index,Count)-> Double-> Bool]-> [FilePath]-> Int-> FilePath-> IO ()
score criteria sources n destination = forM_ sources $ \source-> (readFile source >$ scoring criteria n) >>= writeFile (destination </> takeFileName source)

readFiles :: [String]-> IO [CSV]
readFiles filenames = mapM readFile filenames

readCSVs :: IO [CSV]
readCSVs = getArgs >>= readFiles
