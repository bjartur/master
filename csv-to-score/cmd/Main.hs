module Main where
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.List (isPrefixOf)
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

parseFlags:: [String]-> [[Double] -> (Index, Count) -> Double -> Bool]
parseFlags flags = case flags of {
  ["--unabrupt"] -> [];
  ["--simple"]   -> [];
  ["--reversal"] -> [abrupt];
  ["--medium"]   -> [abrupt];
  ["--baseline"] -> [belowBaseline,abrupt];
  ["--complex"]  -> [belowBaseline,abrupt];
  _              -> error "Unrecognized commandline flag!"
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

  if paths `fewerThan` 2 || length longOptions > 1 || length shortOptions > 1
  then mapM_ putStrLn ["csv2score version 1"
                      , "Usage: csv2score [--unabrupt|--reversal|--baseline] [-nN] [--] DESTINATION FILE..."
                      , "If no method is specified, baseline is used by default."
                      , "Reversal drops the requirement that every nadir be under baseline."
                      , "Unabrupt additionally drops the requirement that a crescendo be followed by an nadir above baseline."
                      , "N is the minimum number of increases in negative pressure. By default, N is 0 which represents varying the the minimum number  from 2 to 5."]
  else forM_ (parseOptions shortOptions) $ \n-> score (parseFlags longOptions) (tail paths) n (head paths </> show n)

score:: [[Double]-> (Index,Count)-> Double-> Bool]-> [FilePath]-> Int-> FilePath-> IO ()
score criteria sources n destination = forM_ sources $ \source-> (readFile source >$ scoring criteria n) >>= writeFile (destination </> takeFileName source)

readFiles :: [String]-> IO [CSV]
readFiles filenames = mapM readFile filenames

readCSVs :: IO [CSV]
readCSVs = getArgs >>= readFiles
