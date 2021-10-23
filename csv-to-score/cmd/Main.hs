module Main where
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.List (intercalate, isPrefixOf, sort)
import Input (CSV, Count, Index, abrupt, belowBaseline, reversal, timestampsOfDeclineBeginning, timestampsOfDeclineEnd, (>$), (>>$))
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

parseFlags:: [String]-> [[Double] -> (Index, Count) -> Double -> Bool]
parseFlags flags = case sort flags of {
  ["--unabrupt"]	-> [];
  ["--simple"]	-> [];
  ["--abrupt"]	-> [abrupt];
  ["--reversal"]	-> [reversal];
  ["--medium"]	-> [abrupt];
  ["--baseline"]	-> [belowBaseline];
  []	-> [];
  ["--baserev"]	-> [belowBaseline,reversal];
  ["--baseline", "--reversal"]	-> [belowBaseline,reversal];
  ["--abrupt", "--baseline"]	-> [belowBaseline,abrupt];
  ["--complex"]	-> [belowBaseline,abrupt];
  _	-> error $ "Unrecognized commandline flags " ++ intercalate " " flags ++ "!"
}

parseOptions:: [String]-> [Int]
parseOptions ['-':'n':number] = if number == "0" then [2..5] else [read number :: Int]
parseOptions [] = [2..5]
parseOptions _ = error "Unrecognized commandline option!"

main :: IO ()
main = do
  args <- getArgs
  let longOptions = takeWhile ("--" `isPrefixOf`) args & takeWhile ("--" /=)
  let shortOptions = takeWhile ("--" /=) args & dropWhile ("--" `isPrefixOf`) & takeWhile ("-" `isPrefixOf`)

  let positionals = dropWhile (\argument-> argument /= "--" && "-" `isPrefixOf` argument) args
  let paths = (if take 1 positionals == ["--"] then drop 1 else id) positionals

  if paths `fewerThan` 2 || length longOptions > 2 || length shortOptions > 1
  then mapM_ putStrLn ["csv2score version 1"
                      , "Synopsis: csv2score --simple [-nN] [--] DESTINATION FILE..."
                      , "Synopsis: csv2score [--baseline] [--reversal|--abrupt] [-nN] [--] DESTINATION FILE..."
                      , "Baseline requires a dip below baseline."
                      , "Reversal requires an return above baseline."
                      , "Aprupt requires an abrupt return above baseline."
                      , "Simple requires none of those things."
                      , "N is the minimum number of successively more negative peak-inspiratory pressures. By default, N is 0 which represents varying the the minimum number  from 2 to 5."]
  else forM_ (parseOptions shortOptions) $ \n-> score (parseFlags longOptions) (tail paths) n (head paths </> show n)

score:: [[Double]-> (Index,Count)-> Double-> Bool]-> [FilePath]-> Int-> FilePath-> IO ()
score criteria sources n destination = forM_ sources $ \source-> (readFile source >$ scoring criteria n) >>= writeFile (destination </> takeFileName source)

readFiles :: [String]-> IO [CSV]
readFiles filenames = mapM readFile filenames

readCSVs :: IO [CSV]
readCSVs = getArgs >>= readFiles
