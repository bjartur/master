module Input (module Input, module Lib) where
import Control.Applicative
import Control.Monad
import Lib

type CSV = String

nadirs :: Int-> CSV-> [(Index,Count)]
nadirs n =           readLatterColumnAsDoubles
                >$ declinesLongerThan n

timestampsOfDeclineBeginning :: Int-> String-> [String]
timestampsOfDeclineBeginning n =
                   timestamp indexBefore n


timestampsOfDeclineEnd :: Int-> String-> [String]
timestampsOfDeclineEnd n =
                   timestamp indexOfEndOf n

baselines :: [(Index,Count)]-> [Double]-> [Double]
baselines candidates pressures = let
            beginIndices = candidates >$ indexBefore
            afterIndices = candidates >$ indexAfter
            selectors = zipWith range (0:afterIndices) beginIndices :: [ [Double]-> [Double] ]
    in
            selectors
         >$ ($ pressures)
         >$ average


abruptNadirs :: Int-> CSV-> [(Index,Count)]
abruptNadirs n = liftA2 abrupts (nadirs n) readLatterColumnAsDoubles

abrupts :: [(Index,Count)]-> [Double]-> [(Index,Count)]
abrupts candidates pressures =
    zip [1..] candidates
 >>=ap abrupt (baselines candidates) pressures

timestamp :: ((Index,Count)-> Index)-> Int-> String-> [String]
timestamp accessor n csv =
                   nadirs n csv
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
