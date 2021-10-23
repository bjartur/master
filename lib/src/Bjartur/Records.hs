{-# LANGUAGE RankNTypes #-}
module Bjartur.Records where

import Control.Applicative( liftA2 )
import Control.DeepSeq ( ($!!) )
import Control.Lens.Setter( (.~), mapped, over )
import Control.Lens.Tuple( _1, _2 )
import Control.Monad ( forM )
import Data.Functor( (<&>) )
import Data.Function( (&), on )
import Data.List( sort, transpose )
import System.Directory ( listDirectory )
import System.FilePath( (</>), splitDirectories, takeFileName )
import qualified Data.IntervalSet  as IntervalSet

import Bjartur.Time
import Bjartur.CSV ( parse )

-- a bit like a Cartesian product, but concatenating instead of pairing
(+++):: [[a]]-> [[a]]-> [[a]]
(+++)= liftA2 (++)

(+/+):: [FilePath]-> [FilePath]-> [FilePath]
(+/+)= liftA2 (</>)
infixr 6 +/+ -- one tighter than ++

transposeLabeled:: [(String, [a])]-> [[(String, a)]]
transposeLabeled= (map $ \(label, row)-> map ((,) label) row)
              <&> transpose

-- (was countAllLines)
-- Given a score directory, return a label and list of all
-- applicable CSV files it contains.
score :: FilePath -> IO (String, [FilePath])
score directory= do
  scorePaths <- listDirectory directory <&> sort
  let enumerated :: [(Int,FilePath)]
      enumerated = pairWith fromScoreName scorePaths
  let forbid = on (liftA2 (&&)) (/=)
  let filtered = filter (fst <&> forbid 13 14) enumerated
  let filenames = filtered <&> snd <&> (directory </>)
  let label :: String
      label = directory & splitDirectories & ((length <&> (subtract 2)) >>= drop) <&> rename & concat
  let values :: IO [FilePath]
      values = pure filenames
  values <&> (,) label

-- Gathers scores from ../csv-to-score using `score`
autoscores :: IO [(String, [FilePath])]
autoscores = do
  let expandedPaths =
        "../csv-to-score/output/unabrupt/10sec/"
        :
        ["../csv-to-score/output/"]
        +/+ ["baseline", "baserev", "complex"]
        +/+ map pure ['2'..'5']
  forM expandedPaths score

-- Gathers scores for KAÓ form ../Nox2score
kao :: IO (String, [FilePath])
kao = score "../Nox2score/output/KAÓ/" <&> (_1 .~ "technologist")
marta:: IO (String, [FilePath])
marta= score "../Nox2score/output/Marta" <&> (_1 .~ "technician")
manual :: IO [(String, [FilePath])]
manual = sequence [kao, marta]

same :: (Eq a, Show a)=> [a]-> Bool
same (eq:eqs) = dropWhile (eq ==) eqs & map (show <&> ( ++ "/=" ++ show eq) <&> error) & and; same [] = True

correctly :: [(String, [FilePath])]-> Bool
correctly sorted= do
  let filenames = sorted <&> snd <&> map takeFileName <&> (map $ take $ length "VSN-14-080-0NN.")
  let equinumberous = same $ map length filenames
  let concordant = transpose filenames & all same
  equinumberous && concordant

-- Paths to all CSV files grouped by PES classifier
-- Joins `autoscores'`, `kao` and `marta`
scores :: IO [(String, [FilePath])]
scores = do
  unsorted <- liftA2 (++) autoscores manual
  let sorted = over (mapped._2) sort unsorted
  if correctly sorted then return sorted else error "Record mismatch!"

numbers :: IO [(String, [PathLines])]
numbers = scores >>=
    mapM (mapM (mapM countLines))
--  ^- map outer list
--        ^- map snd of tuple
--              ^- map list of PathLines

-- Like `autoscores` but with line counts
autoscoresLines :: IO [(String, [PathLines])]
autoscoresLines = autoscores >>= mapM (mapM (mapM countLines))

-- Like `marta` but with line counts
martaLines :: IO (String, [PathLines])
martaLines = marta >>= mapM (mapM countLines)

kaoLines :: IO (String, [PathLines])
kaoLines = kao >>= mapM (mapM countLines)

listIntervals :: IO [(String, [FilePath])] -> IO [(String, Events)]
listIntervals = (>>= mapM ( mapM $ \paths ->
  mapM readIntervals paths <&> IntervalSet.unions ))

-- For every pattern, a list of IntervalSets. One IntervalSet per recording.
getEventsByRecordingByPattern :: IO [(String, [Events])]
getEventsByRecordingByPattern = scores >>= (mapM . mapM . mapM) readIntervals

-- One big IntervalSet out of all CSV files for each classifier
intervals :: IO [(String, Events)]
intervals = listIntervals scores

-- -||- but only autoscores
autoscoredIntervals :: IO [(String, Events)]
autoscoredIntervals = listIntervals autoscores

countLines :: FilePath -> IO PathLines
countLines path = do
  contents <- readFile path
  let lineCount = length . lines $ contents
  pure $!! (path, lineCount)

-- Number of participant from CSV filename
fromScoreName:: FilePath-> Int
fromScoreName= takeFileName <&> drop (length "VSN-14-080-0") <&> take 2 <&> read

pairWith:: (a-> b)-> [a]-> [(b,a)]
pairWith f= (map f >>= zip)

rename:: String-> String
rename "unabrupt"= "cres"
rename "reversal"= "cresrev"
rename "abrupt"= "cresabrupt"
rename "baseline"= "cresbase"
rename "baserev"= "cresbaserev"
rename "complex"= "cresbaseabrupt"
rename other= other

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

readIntervals :: FilePath -> IO Events
readIntervals path = readFile path <&> parse

-- Represent number of lines in a file
type PathLines = (FilePath, Int)
