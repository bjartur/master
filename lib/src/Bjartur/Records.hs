module Bjartur.Records where

import Control.Applicative( liftA2 )
import Control.DeepSeq ( ($!!) )
import Control.Lens.Setter( (.~), (.=), mapped, over, set )
import Control.Lens.Tuple( _1, _2 )
import Control.Monad ( forM )
import Data.Functor( (<&>) )
import Data.Function( (&), on )
import System.Directory ( listDirectory )
import System.FilePath( (</>), splitDirectories, takeFileName, takeBaseName )

import Bjartur.Types
import Bjartur.CSV ( parse )

-- a bit like a Cartesian product, but concatenating instead of pairing
(+++):: [[a]]-> [[a]]-> [[a]]
(+++)= liftA2 (++)

(+/+):: [FilePath]-> [FilePath]-> [FilePath]
(+/+)= liftA2 (</>)
infixr 6 +/+ -- one tighter than ++

numbers:: IO [(String, [PathLines])]
numbers= liftA2 (++) autoscores manual

countLines :: FilePath -> IO PathLines
countLines path = do
  contents <- readFile path
  let lineCount = length . lines $ contents
  pure $!! (path, lineCount)

-- Number of participant from CSV filename
fromScoreName:: FilePath-> Int
fromScoreName= takeFileName <&> drop (length "VSN-14-080-0") <&> take 2 <&> read

manual:: IO [(String, [PathLines])]
manual= sequence [kao, marta]

autoscores:: IO [(String, [PathLines])]
autoscores= do
  let expandedPaths =
        ["../csv-to-score/output/"]
        +/+ ["unabrupt", "reversal", "baseline"]
        +/+ map pure ['2'..'5']
  forM expandedPaths $ \path -> do
    (a,b) <- countAllLines path
    return (a,b)

kao:: IO (String, [PathLines])
kao= countAllLines "../Nox2score/output/KAÓ/" <&> (_1 .~ "technologist")

marta:: IO (String, [PathLines])
marta= countAllLines "../Nox2score/output/Marta" <&> (_1 .~ "technician")

pairWith:: (a-> b)-> [a]-> [(b,a)]
pairWith f= (map f >>= zip)

rename:: String-> String
rename "unabrupt"= "Simple"
rename "reversal"= "Medium"
rename "baseline"= "Complex"
rename other= other

countAllLines :: FilePath -> IO (String, [(FilePath, Int)])
countAllLines directory= do
  scorePaths <- listDirectory directory
  let enumerated :: [(Int,FilePath)]
      enumerated = pairWith fromScoreName scorePaths
  let forbid = on (liftA2 (&&)) (/=)
  let filtered = filter (fst <&> forbid 13 14) enumerated
  let filenames = filtered <&> snd <&> (directory </>)
  let label = directory & splitDirectories & ((length <&> (subtract 2)) >>= drop) <&> rename & concat
  let values :: IO [PathLines]
      values = traverse countLines filenames
  values <&> (,) label

tally:: [(String, [PathLines])]
            -> [(String, [Double])]
tally= over (mapped._2.mapped) (fromIntegral.snd)

-- Data on the length of polysomnograms
--  TST = Total Sleep Time (for each record)
-- the total total is around 167 hours.
tst :: String -- polysomnogram name
    -> Double -- length in hours
tst "VSN-14-080-001" = 7 + 23/60
tst "VSN-14-080-005" = 6 + 53/60
tst "VSN-14-080-006" = 4 + 55/60
tst "VSN-14-080-007" = 6 + 38/60
tst "VSN-14-080-008" = 5 +  3/60
tst "VSN-14-080-009" = 5 + 41/60
tst "VSN-14-080-010" = 6 + 33/60
tst "VSN-14-080-004" = 6 + 58/60
tst "VSN-14-080-003" = 7 + 8/60
tst "VSN-14-080-011" = 1 + 44/60
tst "VSN-14-080-012" = 6 + 16/60
tst "VSN-14-080-015" = 4 + 23/60
tst "VSN-14-080-016" = 7 + 24/60
tst "VSN-14-080-017" = 7 + 40/60
tst "VSN-14-080-018" = 5 + 32/60
tst "VSN-14-080-019" = 7 +  5/60
tst "VSN-14-080-020" = 6 + 49/60
tst "VSN-14-080-021" = 7 + 36/60
tst "VSN-14-080-022" = 7 + 19/60
tst "VSN-14-080-023" = 7 + 15/60
tst "VSN-14-080-024" = 6 + 58/60
tst "VSN-14-080-025" = 6 + 56/60
tst "VSN-14-080-026" = 6 + 59/60
tst "VSN-14-080-027" = 5 + 41/60
tst "VSN-14-080-028" = 7 + 26/60
tst "VSN-14-080-029" = 6 + 27/60
tst "total" = 166.7
tst f = error $ "Weight for recording name " ++ f ++ " is not defined"

-- (was countAllLines)
-- Given a score directory, return a label and list of all
-- applicable CSV files it contains.
score :: FilePath -> IO (String, [FilePath])
score directory= do
  scorePaths <- listDirectory directory
  let enumerated :: [(Int,FilePath)]
      enumerated = pairWith fromScoreName scorePaths
  let forbid = on (liftA2 (&&)) (/=)
  let filtered = filter (fst <&> forbid 13 14) enumerated
  let filenames = filtered <&> snd <&> (directory </>)
  let label :: String
      label = directory & splitDirectories & ((length <&> (subtract 2)) >>= drop) <&> rename & concat
  let values :: IO [FilePath]
      values = traverse pure filenames
  values <&> (,) label

-- (was autoscores)
autoscores' :: IO [(String, [FilePath])]
autoscores' = do
  let expandedPaths =
        ["../csv-to-score/output/"]
        +/+ ["unabrupt", "reversal", "baseline"]
        +/+ map pure ['2'..'5']
  forM expandedPaths $ \path -> do
    (a,b) <- score path
    return (a,b)

readIntervals :: FilePath -> IO (String, Intervals)
readIntervals "marta" = do
  intervals <- parse <$> readCSVDirectory "../Nox2score/output/Marta"
  return ("marta", intervals)
readIntervals "kao"  = do
  intervals <- parse <$> readCSVDirectory "../Nox2score/output/KAÓ"
  return ("kao", intervals)
readIntervals path = do
  contents <- readFile path
  let name = takeFileName path
      parsed = parse contents
  return (name,parsed)

-- concatenated contents of all csv files in a directory
-- EXCEPT those that end with 013.csv and 014.csv
readCSVDirectory :: String -> IO String
readCSVDirectory dir = do
  csvs <- filter forbidden <$> listDirectory dir
  readed <- mapM (\path -> readFile (dir </> path)) csvs
  return $ concat readed
  where
    forbidden :: String -> Bool
    forbidden s =
      let end = snd $ splitAt (length s - 7) s 
        in not $ end == "013.csv" || end == "014.csv"
