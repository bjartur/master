module Main where
import Input

--contents = glob >>= mapM (lines.readFile)
instance Input IO where
        csv = readFile "C:/Users/Bjartur/Master/NoxPes2Csv/nadir/BbB/20141111T235541 - 57128.txt"
main :: IO ()
main = return ()
