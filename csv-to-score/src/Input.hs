module Input (module Input, module Lib) where
import Control.Applicative
import Control.Monad
import Lib

type CSV = String

nadirs :: CSV-> [(Index,Count)]
nadirs =           readLatterColumnAsDoubles
                >$ declinesLongerThanThree

timestampsOfDeclineBeginning :: String-> [String]
timestampsOfDeclineBeginning =
                   timestamp indexBefore


timestampsOfDeclineEnd :: String-> [String]
timestampsOfDeclineEnd =
                   timestamp indexOfEndOf

baselines :: [(Index,Count)]-> [Double]-> [Double]
baselines candidates pressures = let
            beginIndices = candidates >$ indexBefore
            afterIndices = candidates >$ indexAfter
            selectors = zipWith range (0:afterIndices) beginIndices :: [ [Double]-> [Double] ]
    in
            selectors
         >$ ($ pressures)
         >$ average


abruptNadirs :: CSV-> [(Index,Count)]
abruptNadirs = liftA2 abrupts nadirs readLatterColumnAsDoubles

abrupts :: [(Index,Count)]-> [Double]-> [(Index,Count)]
abrupts candidates pressures =
    zip [1..] candidates
 >>=ap abrupt (baselines candidates) pressures

timestamp :: ((Index,Count)-> Index)-> String-> [String]
timestamp accessor csv =
                   nadirs csv
                >$ accessor
                >$ getTimestamps csv

readLatterColumnAsDoubles :: CSV-> [Double]
readLatterColumnAsDoubles = column latterColumn >>$ read

getTimestamps :: CSV-> Index-> String
getTimestamps = readFormerColumn >$ (!!)

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
