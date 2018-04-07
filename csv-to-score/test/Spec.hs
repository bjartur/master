import Lib (increasing)
import Test.Hspec
import Test.QuickCheck
import Data.Function ((&))
import Data.List (sort)


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
        function (sort list) == map (const True) list

ofDescendingListShouldHaveAllFalseTail =
        it "returns True cons a replicate of False for a descending list"
        . property
        . mapsDescendingTailToAllFalseTail

mapsDescendingTailToAllFalseTail :: Ord a =>
        ([a] -> [Bool]) -> NonEmptyList a -> Bool
mapsDescendingTailToAllFalseTail function (NonEmpty list) =
        function (reverse (sort list)) == True : map (const False) (tail list)

main :: IO ()
main = hspec $ do
        describe "increasing" $ do
                mapM_ ($ increasing) [
                        shouldMapTheFirstElementToTrue,
                        shouldPreserveListLength,
                        ofAscendingListShouldBeAllTrue,
                        ofDescendingListShouldHaveAllFalseTail
                        ]
                
        describe "Three breaths each with a lower pressure than a preceding breath" $ do
                return ()
