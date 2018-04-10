import Lib
import Test.Hspec
import Test.QuickCheck (Arbitrary, NonEmptyList(..), Property, property)
import Data.Function ((&))
import Data.List (sort, nub)


shouldPreserveListLength = it "returns a list of length equal to the input list."
        . toPreserveListLength

toPreserveListLength :: (Arbitrary a, Show a) => ([a] -> [b]) -> Property
toPreserveListLength = property . preservesListLength

preservesListLength :: ([a] -> [b]) -> [a] -> Bool
function `preservesListLength` list = length (function list) == length list


shouldMapTheFirstElementToTrue = it "maps the first element, if any, to True."
        . toMapTheFirstElementToTrue

toMapTheFirstElementToTrue :: (Arbitrary a, Show a) => ([a] -> [Bool]) -> Property
toMapTheFirstElementToTrue = property . mapsTheFirstElementToTrue

mapsTheFirstElementToTrue :: ([a] -> [Bool]) -> NonEmptyList a -> Bool
function `mapsTheFirstElementToTrue` (NonEmpty list) = head (function list)

ofAscendingListShouldBeAllTrue =
        it "returns a replicate of True for an ascending list"
        . toBeAllTrueForAscendingList

toBeAllTrueForAscendingList :: (Arbitrary a, Ord a, Show a) => ([a] -> [Bool]) -> Property
toBeAllTrueForAscendingList = property . isAllTrueForAscendingList

isAllTrueForAscendingList :: Ord a => ([a] -> [Bool]) -> [a] -> Bool
isAllTrueForAscendingList function list =
        function(sort list) == map(const True) list

ofDescendingListShouldHaveAllFalseTail =
        it "returns True cons a replicate of False for a descending list"
        . property
        . mapsDescendingTailToAllFalseTail

mapsDescendingTailToAllFalseTail :: Ord a =>
        ([a] -> [Bool]) -> NonEmptyList a -> Bool
mapsDescendingTailToAllFalseTail function (NonEmpty list) =
        function(reverse(sort list)) == True : map(const False) (tail list)

shouldMorphReversalIntoNegation =
        it "preservers reversal for lists without duplicates"
        . property
        . morphsReversalIntoNegation

morphsReversalIntoNegation :: ([Double] -> [Bool]) -> NonEmptyList Double -> Bool
function `morphsReversalIntoNegation` (NonEmpty list) =
        do let filtered = nub list
           let allButFirst = tail . function
           reverse(allButFirst filtered) == map not (allButFirst(reverse filtered))

shouldShortenListByOne =
        it "returns a list one item shorter than input"
        . property
        . shortensAListByOne

shortensAListByOne :: ([a] -> [b]) -> NonEmptyList a -> Bool
function `shortensAListByOne` (NonEmpty list) = length(function list) + 1 == length list

main :: IO ()
main = hspec $ do
        describe "increasing" $ do
                mapM_($ increasing) [
                        shouldMapTheFirstElementToTrue,
                        shouldPreserveListLength,
                        ofAscendingListShouldBeAllTrue,
                        ofDescendingListShouldHaveAllFalseTail,
                        shouldMorphReversalIntoNegation
                        ]
                it "returns three times True and ten times False given [1..3] ++ [2,1..(-7)]" 
                       $         increasing([1..3] ++ [2,1..(-7)])
                       `shouldBe`(replicate 3 True ++ replicate 10 False)
        describe "diffs" $ do
                mapM_($ diffs) [
                        shouldShortenListByOne
                        ]
        describe "Three breaths each with a lower pressure than a preceding breath" $ do
                return ()
