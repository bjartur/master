import Control.Arrow
import Control.Applicative
import Control.Monad(ap, join)
import Data.Function
import Data.Functor.Const
import Data.List (sort, nub, foldl1')
import Debug.Trace
import Input
import Lib
import Prelude hiding (readFile)
import Test.Hspec
import Test.QuickCheck (Arbitrary(..), Arbitrary1(..), choose, forAll, Gen(..), maxSuccess, NonEmptyList(..), Property, property, stdArgs, Testable, quickCheckWith, (==>))


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
        it "preserves reversal for lists without duplicates."
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

returnsInitIndices = do
        it "only returns indices in init."
      . property
      . initIndexOnly

initIndexOnly :: ([a] -> [(Int,Int)]) -> [a] -> Bool
function `initIndexOnly` list = do
        (function list >$ fst)
  <**> [
           (>= 0),
           \index-> length list - index >= 2
       ]
     & and

returnsCountsCappedAtInputLength = do
  it "never finds a span longer than its superlist."
      . property
      . countCappedAtInputLength

countCappedAtInputLength :: ([a] -> [(Int,Int)]) -> [a] -> Bool
function `countCappedAtInputLength` list = do
       (function list >$ snd)
   <**> [ (1 <=),
         (length list >=)
        ]
      & and


uncreative :: ( [Double]-> [(a,(Index,Count))]-> [(Index,Count)] )-> [(a,(Index,Count))]-> Bool
uncreative f input = (f (map (const 8.5) input) `subsets` map snd) input

f `subsets` g = \input-> f input `subset` g input

xs `subset` ys = all (`elem` ys) xs


ofEqualLength =
        returnEqualLength
    >>$ property
    >>$ it "calculates as many baselines as there are nadirs"

returnEqualLength :: ([Double]-> [a])-> ([Double]-> [b])-> [Double]-> Bool
returnEqualLength f g x = length (f x) == length (g x)

returnTheSame :: Eq b=> (a-> b)-> (a-> b)-> a-> Bool
returnTheSame f g x = f x == g x

randomNadir ::
    Int-> -- beginning; the generated nadir will start *after* this index
    Int-> -- recordLength; at minimum beginning + 4
    Gen (Index,Count)
randomNadir beginning recordLength = do
        start <-choose(beginning, (recordLength-1)-3-1)
        count <-choose(3, (recordLength-1)-start-1)
        return (start,count)

randomNadirs :: Int-> Gen [(Index,Count)]
randomNadirs recordLength = let
            go index nadirs = if index > (recordLength-1)-3-1
                then nadirs
                else do
                         (nextIndex,count) <- randomNadir index recordLength
                         list <- nadirs
                         go (nextIndex+count) $ return $ (nextIndex,count):list
        in
            go 1 (return [])
         >$ reverse

examples :: (Show a, Show b, Eq b) => (a -> b) -> [(a,b)] -> Expectation
examples function = mapM_(\(input,output) -> function input `shouldBe` output)

abcdef = "a,b\r\ncd,ef\r\n"

csvContainingNumbers = "former column,-2.894930373863120749e-02\r\nformer column,-7.567405304247912341e-02"

longCsv = "former column,-7.567405304247912341e-02\r\nformer column,-7.564403304247913341e-02\r\n"
         ++ "former column,1.2\r\nformer column,1.1\r\nformer column,1.0\r\nformer column,-3.1e-01\r\n"
longerCsv = "former column,-7.567405304247912341e-02\r\nformer column,-7.564403304247913341e-02\r\n"
        ++ "former column,1.2\r\nformer column,1.1\r\nformer column,1.0\r\nformer column,-3.1e-01\r\n"
         ++ "former column,-3.14e-01\r\nformer column,-4.564403304247e-01\r\n"
         ++ "former column,1.2\r\nformer column,1.1\r\nformer column,1.0\r\nformer column,-3.1e-01\r\n"

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
                             , (1:[0..5]++[4..7],False:replicate 5 True++False:replicate 3 True)
                        ]
        describe "spans" $ do
                it "finds and measures spans of Trues." $ examples spans
                                [
                                        ([],[])
                                      , ([True,True,False,True],[(0,2),(3,1)])
                                      , ([False,True,False,True,True,True,True],[(1,1),(3,4)])
                                ]
        describe "rises" $
                ofAscendingListShouldContainOnlyZeroAndLength rises
        describe "Three breaths each with a higher pressure than a preceding breath" $ do
                it "finds no rise in an empty list." $
                        risesLongerThanThree[] `shouldBe` []
                it "skips over an increasing span of length three as well as a decrease." $
                        risesLongerThanThree([1..3] ++ [2,1..(-7)]) `shouldBe` []
                it "identifies long rises" $ do
                        quickCheckWith stdArgs { maxSuccess = 5000 } $ examples risesLongerThanThree
                                [
                                        ([1..10],[(0,9)])
                                      , (1:[0..5]++[4..7],[(1,5),(7,3)])
                                ]
                returnsInitIndices risesLongerThanThree
                mapM_($ risesLongerThanThree)
                        [
                                returnsInitIndices
                              , returnsCountsCappedAtInputLength
                              , it "of [1..4] returns no index greater than 2"
                                . shouldSatisfy [1..4]
                                . initIndexOnly
                              , it "of [1..4] returns no index greater than 2"
                                . shouldSatisfy [1..4]
                                . countCappedAtInputLength
                        ]
                it "has been parametrized" $ do
                  property $ returnTheSame risesLongerThanThree (risesLongerThan 3)
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
                                returnsInitIndices
                              , returnsCountsCappedAtInputLength
                        ]
                it "has been parametrized" $ do
                  property $ returnTheSame declinesLongerThanThree (declinesLongerThan 3)
        describe "readFormerColumn" $ do
                it "extracts strings with and without spaces from small CSV files." $ do
                        readFormerColumn abcdef `shouldBe` ["a","cd"]
                        readFormerColumn csvContainingNumbers `shouldBe` replicate 2 "former column"
        describe "readLatterColumn" $ do
                it "extracts numbers in scientific notation from small CSV files." $ do
                        readLatterColumnAsDoubles csvContainingNumbers `shouldBe` [-2.894930373863120749e-02,-7.567405304247912341e-02]
        describe "baselines" $ do
                let pressures  = [-7.567405304247912341e-02,-7.564403304247913341e-02,1.2,1.1,1.0,-3.1e-01]
                let candidates = declinesLongerThanThree pressures
                let references = baselines candidates pressures
                it "calculates a mean before a decline." $ do
                    shouldBe
                       (
                            references
                         >$(\outcome->
                                   (outcome
                                  / 0.34956063797168058106
                                  - 1
                                  & abs)
                                  < 0.001
                           )
                          & liftA2 (,) and length
                       )
                        (True, 1)
                ofEqualLength declinesLongerThanThree (\pressures-> baselines (declinesLongerThanThree pressures) pressures)
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
                it "only accepts or rejects candidate nadirs without introducing new ones" $
                       (forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property)
                       (   randomNadirs 19
                         >$ zip [(0::Int)..]
                       )
                       (uncreative $ \references candidates->
                                do
                                        candidate <-candidates
                                        abrupt [8, 9, 8, 9, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 9, 9, 10, 9, 8] references candidate
                       )
