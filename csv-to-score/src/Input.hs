module Input( CSV, decrescendoBelowBaselineTerminatedByReversal, timestampsOfDeclineBeginning, timestampsOfDeclineEnd, readFormerColumn, readLatterColumnAsDoubles, (>$), (>>$) ) where
import Control.Applicative
import Lib

type CSV = String

decrescendos :: Int-> CSV-> [(Index,Count)]
decrescendos n =           readLatterColumnAsDoubles
                >$ declinesLongerThan n

decrescendoBelowBaselineTerminatedByReversal :: Int-> CSV-> [(Index,Count)]
decrescendoBelowBaselineTerminatedByReversal n = liftA2 (baseline n) readLatterColumnAsDoubles (decrescendos n)

timestampsOfDeclineBeginning :: Int-> String-> [String]
timestampsOfDeclineBeginning n =
                   timestamp indexBefore n


timestampsOfDeclineEnd :: Int-> String-> [String]
timestampsOfDeclineEnd n =
                   timestamp indexOfEndOf n

timestamp :: ((Index,Count)-> Index)-> Int-> String-> [String]
timestamp accessor n csv =
                   decrescendoBelowBaselineTerminatedByReversal n csv
                >$ accessor
                >$ getTimestamps csv

readLatterColumnAsDoubles :: CSV-> [Double]
readLatterColumnAsDoubles = column latterColumn >>$ read

getTimestamps :: CSV-> Index-> String
getTimestamps = readFormerColumn >$ (!!) >>$ appendMicrosecondsIfMissing

appendMicrosecondsIfMissing:: String-> String
appendMicrosecondsIfMissing datetime=
  datetime ++ if length datetime == length "YYYY-MM-DD HH:MM:SS" then ".000000" else ""

readFormerColumn :: CSV-> [String]
readFormerColumn = column formerColumn

type Column = (Char-> Bool)-> String-> String

column :: Column-> CSV-> [String]
column selectColumn csv =
                   lines csv
                >$ selectColumn (/= ',')

formerColumn :: Column
formerColumn = firstColumn

firstColumn :: Column
firstColumn = takeWhile

latterColumn :: Column
latterColumn = dropFirstColumn

dropFirstColumn :: Column
dropFirstColumn = dropWhile
          >>$ tail
