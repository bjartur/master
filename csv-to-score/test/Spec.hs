import Lib (increasing)
import Test.Hspec
import Test.QuickCheck
import Data.Function ((&))

preservesListLength :: ([a] -> [a]) -> [a] -> Bool
function `preservesListLength` list = length (function list) == length list

toPreserveListLength :: (Arbitrary a, Show a) => ([a] -> [a]) -> Property
toPreserveListLength = property . preservesListLength

shouldPreserveListLength = it "returns a list of length equal to the input list." .
	toPreserveListLength

main :: IO ()
main = hspec $ do
	describe "increasing" $ do
		increasing & shouldPreserveListLength
	describe "Three breaths each with a lower pressure than a preceding breath" $ do
		return ()
