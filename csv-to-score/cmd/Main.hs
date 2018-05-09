module Main where
import Lib
import Control.Applicative
import Control.Arrow

(>$) :: Functor l => l a -> (a -> b) -> l b
(>$) = flip fmap
infixl 1 >$

--contents = glob >>= mapM (lines.readFile)
type Index = Int -- nonnegative
type Count = Int -- positive
type Debt  = Int -- impositive

nadirs :: IO [(Index,Count)]
nadirs =           readLatterColumnAsDoubles
                >$ indicesOfRisesLongerThanThree

timestampsOfRiseBeginning :: IO [String]
timestampsOfRiseBeginning =
                   timestamp
                   (\(index,_)-> index)

timestampsOfRiseEnd :: IO [String]
timestampsOfRiseEnd =
                   timestamp
                   (\(index,count)-> index + count)

csv :: IO String
csv = readFile "C:/Users/Bjartur/Master/NoxPes2Csv/nadir/BbB/20141111T235541 - 57128.txt"

main :: IO ()
main = writeToNewCsvs timestampsOfRiseBeginning

(>>$) :: (Functor l, Functor m) => l (m a) -> (a -> b) -> l (m b)
boxed >>$ function =
                  boxed
               >$ fmap function
infixl 0 >>$

(>>$$) :: (Functor io, Functor list) => io (list a) -> io (a -> b) -> io (list b)
boxedValue >>$$ boxedFunction =
                  boxedFunction >>= \function->
                  boxedValue
              >>$ function
infixl 0 >>$$

timestamp :: ((Index,Count) -> Index) -> IO [String]
timestamp accessor =
                  nadirs
              >>$ accessor
             >>$$       readFormerColumn
                    >$ (!!)

readLatterColumnAsDoubles :: IO [Double]
readLatterColumnAsDoubles = column latterColumn

getTimestamp :: IO (Index -> String)
getTimestamp =    readFormerColumn
              >$ (!!)

readFormerColumn :: IO [String]
readFormerColumn = column formerColumn

type Column = (Char -> Bool) -> String -> String

column :: Read a => Column -> IO [a]
column selectColumn =
                    csv
                 >$ lines
               <**> (selectColumn (/= ',')
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
