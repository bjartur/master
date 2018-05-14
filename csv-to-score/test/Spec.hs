import Control.Applicative
import Data.Function
import Data.List (sort, nub)
import Lib hiding (readFile)
import Prelude hiding (readFile)
import Test.Hspec
import Test.QuickCheck (Arbitrary, NonEmptyList(..), Property, property, (==>))

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

ofAscendingListShouldContainOnlyZero =
                it "finds the only rise in any ascending list"
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

(>$) = flip fmap
infixl 1 >$

onlyReturnsIndicesLowerThanTheLengthOf :: ([a] -> [(Int,Int)]) -> [a] -> Bool
function `onlyReturnsIndicesLowerThanTheLengthOf` list =
        function list
   <**> [fst,snd]
     >$ (< length list)
      & and

examples :: (Show a, Show b, Eq b) => (a -> b) -> [(a,b)] -> Expectation
examples function = mapM_(\(input,output) -> function input `shouldBe` output)

main :: IO ()
main = hspec $ do
        describe "increasing" $ do
                mapM_($ increasing) [
                        shouldShortenListByOne,
                        ofAscendingListShouldBeAllTrue,
                        ofDescendingListShouldBeAllFalse,
                        shouldMorphReversalIntoNegation
                        ]
                it "returns two times True and ten times False given [1..3] ++ [2,1..(-7)]" $
                        examples increasing $
                        [
                               ([1..3] ++ [2,1..(-7)],(replicate 2 True ++ replicate 10 False))
                                ,(1:[0..5]++[4..7],False:replicate 5 True++False:replicate 3 True)
                        ]
        describe "spans" $ do
                it "finds and measures spans of Trues" $ examples spans
                                [
                                        ([],[])
                                        ,([True,True,False,True],[(0,2),(3,1)])
                                        ,([False,True,False,True,True,True,True],[(1,1),(3,4)])
                                ]
              --it "finds the correct number of spans" $
              --        increasing
        describe "rises" $
                ofAscendingListShouldContainOnlyZero rises
        describe "Three breaths each with a lower pressure than a preceding breath" $ do
                it "finds no rise in an empty list" $
                        indicesOfRisesLongerThanThree[] `shouldBe` []
                it "skips over an increasing span of length three as well as a decrease." $
                        indicesOfRisesLongerThanThree([1..3] ++ [2,1..(-7)]) `shouldBe` []
                it "identifies long rises" $ do
                        examples indicesOfRisesLongerThanThree
                                [
                                        ([1..10],[(0,9)]),
                                        (1:[0..5]++[4..7],[(1,5),(7,3)])
                                ]
                mapM_($ indicesOfRisesLongerThanThree)
                        [
                                shouldOnlyReturnIndicesLowerThanTheLengthOf
                        ]
