module Main where
import Control.Applicative (liftA2)
import Control.Monad (forM_)
import Input
import System.Environment (getArgs)
import System.FilePath (takeFileName, (</>))

spliceRow :: String-> String-> String
spliceRow left right = left ++ (',':right)

spliceColumns :: [String]-> [String]-> [String]
spliceColumns = zipWith spliceRow

scores :: Int-> CSV-> [String]
scores n = liftA2 spliceColumns (timestampsOfDeclineBeginning n) (timestampsOfDeclineEnd n)

scoring :: Int-> CSV-> String
scoring = scores >>$ unlines

fewerThan :: [a]-> Int-> Bool
elements `fewerThan` n = null $ drop (n-1) elements

main :: IO ()
main = do
  args <- getArgs
  if args `fewerThan` 2
    then mapM_ putStrLn ["csv2score version 1", "Usage: csv2score DESTINATION FILE..."]
    else score (tail args) 3 (head args </> "breaths" </> show 3)

score:: [FilePath]-> Int-> FilePath-> IO ()
score sources n destination = forM_ sources $ \source-> (readFile source >$ scoring n) >>= writeFile (destination </> takeFileName source)

readFiles :: [String]-> IO [CSV]
readFiles filenames = mapM readFile filenames

readCSVs :: IO [CSV]
readCSVs = getArgs >>= readFiles
