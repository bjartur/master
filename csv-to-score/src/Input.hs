module Input where
import Control.Applicative
import Control.Arrow
import Lib

type Index = Int -- nonnegative
type Count = Int -- positive
type Debt  = Int -- impositive

(>$) :: Functor l => l a -> (a -> b) -> l b
(>$) = flip fmap
infixl 1 >$

nadirs :: Input input => input [(Index,Count)]
nadirs =           readLatterColumnAsDoubles
                >$ indicesOfRisesLongerThanThree

timestampsOfRiseBeginning :: Input input => input [String]
timestampsOfRiseBeginning =
                   timestamp
                   (\(index,_)-> index)

timestampsOfRiseEnd :: Input input => input [String]
timestampsOfRiseEnd =
                   timestamp
                   (\(index,count)-> index + count)

(>>$) :: (Functor l, Functor m) => l (m a) -> (a -> b) -> l (m b)
boxed >>$ function =
                  boxed
               >$ fmap function
infixl 0 >>$

(>>$$) :: (Monad io, Functor list) => io (list a) -> io (a -> b) -> io (list b)
boxedValue >>$$ boxedFunction =
                (boxedValue >>$)
             =<< boxedFunction
infixl 0 >>$$

timestamp :: Input input => ((Index,Count) -> Index) -> input [String]
timestamp accessor =
                  nadirs
              >>$ accessor
             >>$$       getTimestamps

readLatterColumnAsDoubles :: Input input => input [Double]
readLatterColumnAsDoubles = column latterColumn >>$ read

getTimestamps :: Input input => input (Index -> String)
getTimestamps =    readFormerColumn
              >$ (!!)

readFormerColumn :: Input input => input [String]
readFormerColumn = column formerColumn

linesOfCsv :: Input input => input [String]
linesOfCsv =        csv
                 >$ lines

type Column = (Char -> Bool) -> String -> String

column :: Input input => Column -> input [String]
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
        csv :: m String -- wrapped string delimated by newlines and commas.
