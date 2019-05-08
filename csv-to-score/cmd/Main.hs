module Main where
import Control.Applicative
import Input

--contents = glob >>= mapM (lines.readFile)
instance Input IO where
        csv = readFile "C:/Users/Bjartur/Master/NoxPes2Csv/nadir/BbB/20141218T214507 - eb35b.txt"

spliceRow :: String -> String -> String
spliceRow formerCell latterCell = formerCell ++ ',' : latterCell
spliceRows :: [String] -> [String] -> [String]
spliceRows = zipWith spliceRow
spliceRowsFrom :: IO [String] -> IO [String] -> IO [String]
spliceRowsFrom = liftA2 spliceRows
rows :: IO [String]
rows = spliceRowsFrom timestampsOfDeclineBeginning timestampsOfDeclineEnd
output :: IO String
output = rows >$ unlines

main :: IO ()
main = output
   >>= writeFile "output"
