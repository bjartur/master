module Input (module Input, module Lib) where
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Lib

nadirs :: Input input=> input [(Index,Count)]
nadirs =           readLatterColumnAsDoubles
                >$ declinesLongerThanThree

timestampsOfDeclineBeginning :: Input input => input [String]
timestampsOfDeclineBeginning =
                   timestamp indexBefore


timestampsOfDeclineEnd :: Input input=> input [String]
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


abruptNadirs :: Input input=> input [(Index,Count)]
abruptNadirs =
    liftA2 abrupts nadirs readLatterColumnAsDoubles

abrupts :: [(Index,Count)]-> [Double]-> [(Index,Count)]
abrupts candidates pressures =
    zip [1..] candidates
 >>=ap abrupt (baselines candidates) pressures

(>>$$) :: (Monad io, Functor list) => io (list a) -> io (a -> b) -> io (list b)
boxedValue >>$$ boxedFunction =
                   (boxedValue >>$)
               =<< boxedFunction
infixl 2 >>$$

timestamp :: Input input=> ((Index,Count) -> Index) -> input[String]
timestamp accessor =
                   nadirs
               >>$ accessor
              >>$$ getTimestamps

readLatterColumnAsDoubles :: Input input=> input[Double]
readLatterColumnAsDoubles = column latterColumn >>$ read

getTimestamps :: Input input=> input(Index -> String)
getTimestamps =    readFormerColumn
              >$ (!!)

readFormerColumn :: Input input => input[String]
readFormerColumn = column formerColumn

linesOfCsv :: Input input => input [String]
linesOfCsv =        csv
                 >$ lines

type Column = (Char -> Bool) -> String -> String

column :: Input input=> Column -> input[String]
column selectColumn =
                    linesOfCsv
               >>$ selectColumn (/= ',')

formerColumn :: Column
formerColumn = firstColumn

firstColumn = takeWhile

(>>>>) f g x = f x
           >>> g

latterColumn :: Column
latterColumn = dropFirstColumn

dropFirstColumn = dropWhile
          >>>> tail

class Monad m => Input m where
        csv :: m String -- wrapped string delimited by newlines and commas.
