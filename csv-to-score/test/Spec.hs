{-# LANGUAGE DeriveFunctor, DeriveAnyClass #-}
import Control.Applicative
import Data.Function
import Data.Functor.Const
import Data.List (sort, nub, foldl1')
import Input
import Lib
import Prelude hiding (readFile)
import Test.Hspec
import Test.QuickCheck (Arbitrary(..), Arbitrary1(..), NonEmptyList(..), Property, property, (==>))

{-
shouldPreserveListLength = it "returns a list of length equal to the input list."
        . toPreserveListLength

toPreserveListLength :: (Arbitrary a, Show a) => ([a] -> [b]) -> Property
toPreserveListLength = property . preservesListLength

preservesListLength :: ([a] -> [b]) -> [a] -> Bool
function `preservesListLength` list = length (function list) == length list
-}

ofAscendingListShouldBeAllTrue =
        it "returns a replicate of True for an ascending list."
        . toBeAllTrueForAscendingList

toBeAllTrueForAscendingList :: (Arbitrary a, Ord a, Show a) => ([a] -> [Bool]) -> Property
toBeAllTrueForAscendingList = property . isAllTrueForAscendingList

isAllTrueForAscendingList :: Ord a => ([a] -> [Bool]) -> NonEmptyList a -> Bool
isAllTrueForAscendingList function (NonEmpty list) =
        do let filtered = nub list
           function(sort filtered) == tail(map(const True) filtered)

ofAscendingListShouldContainOnlyZeroAndLength =
                it "finds the only rise in any ascending list."
              . property
              . mapsAscendingListToListContainingOnlyZeroAndLength

mapsAscendingListToListContainingOnlyZeroAndLength :: (Eq b, Ord a, Num b) => ([a] -> [(b,Int)]) -> NonEmptyList a -> Property
mapsAscendingListToListContainingOnlyZeroAndLength function (NonEmpty unfiltered) =
        let list = nub unfiltered in
        length list > 1
    ==> function(sort list) == [(0,length list - 1)]

ofDescendingListShouldBeAllFalse =
        it "returns a replicate of False for a descending list."
      . property
      . mapsDescendingListToAllFalse

mapsDescendingListToAllFalse :: Ord a =>
        ([a] -> [Bool]) -> NonEmptyList a -> Bool
mapsDescendingListToAllFalse function (NonEmpty list) =
        function(reverse(sort list)) == map(const False) (tail list)

shouldMorphReversalIntoNegation =
        it "preservers reversal for lists without duplicates."
      . property
      . morphsReversalIntoNegation

morphsReversalIntoNegation :: ([Double] -> [Bool]) -> NonEmptyList Double -> Bool
function `morphsReversalIntoNegation` (NonEmpty list) =
        do let filtered = nub list
           reverse(function filtered) == map not (function $ reverse filtered)

shouldShortenListByOne =
        it "returns a list one item shorter than input."
      . property
      . shortensListByOne

shortensListByOne :: ([a] -> [b]) -> NonEmptyList a -> Bool
function `shortensListByOne` (NonEmpty list) = length(function list) + 1 == length list

shouldOnlyReturnIndicesLowerThanTheLengthOf =
        it "returns neither indices nor counts greater than the length of the input list."
      . property
      . onlyReturnsIndicesLowerThanTheLengthOf

onlyReturnsIndicesLowerThanTheLengthOf :: ([a] -> [(Int,Int)]) -> [a] -> Bool
function `onlyReturnsIndicesLowerThanTheLengthOf` list =
        function list
   <**> [fst,snd]
     >$ (< length list)
      & and

equallyLong :: [b] -> [c] -> Bool
equallyLong xs ys = length xs == length ys

examples :: (Show a, Show b, Eq b) => (a -> b) -> [(a,b)] -> Expectation
examples function = mapM_(\(input,output) -> function input `shouldBe` output)

newtype Abcdef a = Abcdef a deriving (Eq, Show, Functor, Monad, Applicative)
instance Input Abcdef where
        csv = Abcdef "a,b\r\ncd,ef\r\n"

newtype CsvContainingNumbers a = CsvContainingNumbers a deriving (Applicative, Eq, Functor, Monad, Show)
instance Input CsvContainingNumbers where
        csv = CsvContainingNumbers "former column,-2.894930373863120749e-02\r\nformer column,-7.567405304247912341e-02"

newtype LongCsv a = LongCsv a deriving (Applicative, Eq, Functor, Monoid, Show)
instance Input LongCsv where
    csv = LongCsv $ "former column,-7.567405304247912341e-02\r\nformer column,-7.564403304247913341e-02\r\n"
         ++ "former column,1.2\r\nformer column,1.1\r\nformer column,1.0\r\nformer column,-3.1e-01\r\n"
instance Monad LongCsv where
    (LongCsv value) >>= f = f value
    return = LongCsv

newtype LongerCsv a = LongerCsv a deriving (Applicative, Eq, Functor, Monoid, Show)
instance Input LongerCsv where
    csv = LongerCsv $ "former column,-7.567405304247912341e-02\r\nformer column,-7.564403304247913341e-02\r\n"
         ++ "former column,1.2\r\nformer column,1.1\r\nformer column,1.0\r\nformer column,-3.1e-01\r\n"
         ++ "former column,-3.14e-01\r\nformer column,-4.564403304247e-01\r\n"
         ++ "former column,1.2\r\nformer column,1.1\r\nformer column,1.0\r\nformer column,-3.1e-01\r\n"
instance Monad LongerCsv where
    (LongerCsv value) >>= f = f value
    return = LongerCsv

main :: IO ()
main = hspec $ do
        describe "increasing" $ do
                mapM_($ increasing) [
                        shouldShortenListByOne,
                        ofAscendingListShouldBeAllTrue,
                        ofDescendingListShouldBeAllFalse,
                        shouldMorphReversalIntoNegation
                        ]
                it "returns two times True and ten times False given [1..3] ++ [2,1..(-7)]." $
                        examples increasing $
                        [
                               ([1..3] ++ [2,1..(-7)],(replicate 2 True ++ replicate 10 False))
                                ,(1:[0..5]++[4..7],False:replicate 5 True++False:replicate 3 True)
                        ]
        describe "spans" $ do
                it "finds and measures spans of Trues." $ examples spans
                                [
                                        ([],[])
                                        ,([True,True,False,True],[(0,2),(3,1)])
                                        ,([False,True,False,True,True,True,True],[(1,1),(3,4)])
                                ]
        describe "rises" $
                ofAscendingListShouldContainOnlyZeroAndLength rises
        describe "Three breaths each with a higher pressure than a preceding breath" $ do
                it "finds no rise in an empty list." $
                        risesLongerThanThree[] `shouldBe` []
                it "skips over an increasing span of length three as well as a decrease." $
                        risesLongerThanThree([1..3] ++ [2,1..(-7)]) `shouldBe` []
                it "identifies long rises" $ do
                        examples risesLongerThanThree
                                [
                                        ([1..10],[(0,9)]),
                                        (1:[0..5]++[4..7],[(1,5),(7,3)])
                                ]
                mapM_($ risesLongerThanThree)
                        [
                                shouldOnlyReturnIndicesLowerThanTheLengthOf
                        ]
        describe "Three breaths each with a lower pressure than a preceding breath" $ do
                it "finds no rise in an empty list." $
                        declinesLongerThanThree[] `shouldBe` []
                it "skips over an increasing span as well as a decrease of length three." $
                        declinesLongerThanThree([-7..1] ++ [3,2,1]) `shouldBe` []
                it "identifies long declines." $ do
                        examples declinesLongerThanThree
                                [
                                        ([10,9..1],[(0,9)]),
                                        (1:[5,4..0]++[7,6..4],[(1,5),(7,3)])
                                ]
                mapM_($ declinesLongerThanThree)
                        [
                                shouldOnlyReturnIndicesLowerThanTheLengthOf
                        ]
        describe "readFormerColumn" $ do
                it "extracts strings with and without spaces from small CSV files." $ do
                        (readFormerColumn :: Abcdef[String]) `shouldBe` Abcdef["a","cd"]
                        (readFormerColumn :: CsvContainingNumbers[String]) `shouldBe` CsvContainingNumbers(replicate 2 "former column")
        describe "readLatterColumn" $ do
                it "extracts numbers in scientific notation from small CSV files." $ do
                        (readLatterColumnAsDoubles :: CsvContainingNumbers[Double]) `shouldBe` CsvContainingNumbers[-2.894930373863120749e-02,-7.567405304247912341e-02]
        describe "baselines" $ do
                it "calculates a mean before a decline." $ do
                    shouldBe
                        (
                            (baselines :: LongCsv[Double])
                        >>$ (/ (0.34956063797168058106))
                        >>$ (\float-> float-1)
                        >>$ abs
                        >>$ (< 0.001)
                         >$ and
                        )
                        (LongCsv True)
                    shouldBe
                        (
                            (baselines :: LongCsv[Double])
                         >$ length
                        )
                        (LongCsv 1)
                it "can calculate a mean before each decline." $ do
                    shouldBe
                        (
                            (baselines :: LongerCsv[Double])
                         >$ head
                         >$ (/ (0.34956063797168058106))
                         >$ (\float-> float-1)
                         >$ abs
                         >$ (< 0.001)
                        )
                        (LongerCsv True)
                    shouldBe
                        (
                            (baselines :: LongerCsv[Double])
                         >$ length
                        )
                        (LongerCsv 2)
                    shouldBe
                        (
                            (baselines :: LongerCsv[Double])
                            >$ tail
                            >$ head
                            >$ (/ 1.2)
                            >$ (\float-> float-1)
                            >$ abs
                        )
                        (LongerCsv 0)
                it "calculates as many baselines as there are declines." $ do
                    shouldBe
                        (fmap length (baselines :: LongerCsv[Double]))
                        (fmap length (nadirs :: LongerCsv[(Index,Count)]))
        describe "abrupt" $ do
                it "dismisses a triangle." $ do
                        examples (abrupt [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  [10])
                                [
                                        ( (0,(0,9)) , [] )
                                ]
                it "can notice an abrupt return to baseline" $ do
                        examples (abrupt [8, 9, 8, 9, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 9, 9, 10, 9, 8] [8.5])
                                [
                                        ( (0,(4,9)) , [(4,9)] )
                                ]