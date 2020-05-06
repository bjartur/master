module Input( CSV, Count, Index, abrupt, belowBaseline, decrescendosFulfilling, judge, timestampsOfDeclineBeginning, timestampsOfDeclineEnd, readFormerColumn, readLatterColumnAsDoubles, (>$), (>>$) ) where
import Control.Applicative
import Lib

type CSV = String

decrescendos :: Int-> CSV-> [(Index,Count)]
decrescendos n =           readLatterColumnAsDoubles
                >$ declinesLongerThan n

decrescendosFulfilling :: [[Double]-> (Index,Count)-> Double-> Bool]-> Int-> CSV-> [(Index,Count)]
decrescendosFulfilling method n = liftA2 (judge method n)
  readLatterColumnAsDoubles
  (decrescendos n)

timestampsOfDeclineBeginning :: [[Double]-> (Index,Count)-> Double-> Bool]-> Int-> CSV-> [String]
timestampsOfDeclineBeginning =
                   timestamp indexBefore


timestampsOfDeclineEnd :: [[Double]-> (Index,Count)-> Double-> Bool]-> Int-> CSV-> [String]
timestampsOfDeclineEnd =
                   timestamp indexOfEndOf

timestamp :: ((Index,Count)-> Index)-> [[Double]-> (Index,Count)-> Double-> Bool]-> Int-> String-> [String]
timestamp accessor method n csv =
                   decrescendosFulfilling method n csv
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
