module Main where
import Input
import Control.Applicative
import Control.Arrow
import Prelude hiding (readFile)
import Lib

(>$) :: Functor l => l a -> (a -> b) -> l b
(>$) = flip fmap
infixl 1 >$

--contents = glob >>= mapM (lines.readFile)
type Index = Int -- nonnegative
type Count = Int -- positive
type Debt  = Int -- impositive

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

instance Input IO where
        csv = readFile "C:/Users/Bjartur/Master/NoxPes2Csv/nadir/BbB/20141111T235541 - 57128.txt"
main :: IO ()
main = return ()

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
             >>$$       readFormerColumn
                    >$ (!!)

readLatterColumnAsDoubles :: Input input => input [Double]
readLatterColumnAsDoubles = column latterColumn

getTimestamp :: Input input => input (Index -> String)
getTimestamp =    readFormerColumn
              >$ (!!)

readFormerColumn :: Input input => input [String]
readFormerColumn = column formerColumn

linesOfCsv :: Input input => input [String]
linesOfCsv =        csv
                 >$ lines

type Column = (Char -> Bool) -> String -> String

column :: (Input input, Read readable) => Column -> input [readable]
column selectColumn =
                    linesOfCsv
               >>$ (selectColumn (/= ',')
                     >>> read)

formerColumn :: Column
formerColumn = firstColumn

firstColumn = takeWhile

(>>>>) f g x = f x
           >>> g

latterColumn :: Column
latterColumn = dropFirstColumn

dropFirstColumn = dropWhile
          >>>> tail
