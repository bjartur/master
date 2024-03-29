import Control.Applicative
import Control.Monad(ap)
import Data.Function
import Data.List (sort, nub)
import Input
import Lib
import Prelude
import Test.Hspec hiding (after, before)
import Test.QuickCheck (Arbitrary(..), choose, forAll, Gen, listOf, maxSuccess, NonEmptyList(..), Property, property, resize, sized, stdArgs, quickCheckWith, (==>))


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

randomNadirs :: Gen [(Index,Count)]
randomNadirs = sized $ \recordLength-> let
            go index nadirs = if index > (recordLength-1)-3-1
                then nadirs
                else do
                         (nextIndex,count) <- randomNadir index recordLength
                         list <- nadirs
                         go (nextIndex+count) $ return $ (nextIndex,count):list
        in
            go 1 (return [])
         >$ reverse

abrupts :: Int-> [Double]-> [(Index,Count)]-> [(Index,Count)]
abrupts = judge [abrupt]

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

candidateBaseline007 :: IO CSV
candidateBaseline007 = readFile "test/candidateBaseline007.csv"

vsn007 = (>$) candidateBaseline007 (++unlines ["2014-12-03 00:59:46.012000,5.910873415838406586e-02"
  ,"2014-12-03 00:59:48.332000,5.891799929501399802e-02"
  ,"2014-12-03 00:59:50.952000,5.877494814748644714e-02"
  ,"2014-12-03 00:59:53.611500,5.873680117481243357e-02"
  ,"2014-12-03 00:59:56.251000,5.850791933876835216e-02"
  ,"2014-12-03 00:59:59.171000,5.846023562292583520e-02"
  ,"2014-12-03 01:00:01.931000,5.805015566668018934e-02"
  ,"2014-12-03 01:00:04.631000,5.767822268310855705e-02"
  ,"2014-12-03 01:00:07.331000,5.795478823499515542e-02"
    ])

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
                it "identifies long rises." $
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
                it "has been parametrized." $
                  property $ returnTheSame risesLongerThanThree (risesLongerThan 3)
                it "a smaller number of increasing spans can only come by raising the minimum length of each." $
                  property $ \n m pressures-> (length(risesLongerThan n pressures) < length(risesLongerThan m pressures)) ==> (n > m)
        describe "Three breaths each with a lower pressure than a preceding breath" $ do
                it "finds no rise in an empty list." $
                        declinesLongerThanThree[] `shouldBe` []
                it "skips over an increasing span as well as a decrease of length three." $
                        declinesLongerThanThree([-7..1] ++ [3,2,1]) `shouldBe` []
                it "identifies long declines." $
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
                it "has been parametrized" $
                  property $ returnTheSame declinesLongerThanThree (declinesLongerThan 3)
                it "a smaller number of decreasing spans can only come by raising the minimum length of each." $
                  property $ \n m pressures-> (length(declinesLongerThan n pressures) < length(declinesLongerThan m pressures)) ==> (n > m)
        describe "readFormerColumn" $ do
                it "extracts strings with and without spaces from small CSV files." $ do
                        readFormerColumn abcdef `shouldBe` ["a","cd"]
                        readFormerColumn csvContainingNumbers `shouldBe` replicate 2 "former column"
                        readFormerColumn longCsv `shouldBe` replicate 6 "former column"
                        readFormerColumn longerCsv `shouldBe` replicate 12 "former column"
        describe "readLatterColumn" $ do
                it "extracts numbers in scientific notation from small CSV files." $ do
                        readLatterColumnAsDoubles csvContainingNumbers `shouldBe` [-2.894930373863120749e-02,-7.567405304247912341e-02]
        describe "indexAfter" $ do
                it "returns one more than the index of the last breath in a given event" $ do
                        indexAfter (0, 3) `shouldBe` 4
        describe "abrupt" $ do
                it "dismisses a triangle." $ do
                        abrupt [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] (0, 9) [5.5] `shouldBe` False
                it "can notice an abrupt return to baseline" $ do
                        abrupt [8, 9, 8, 9, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 9, 9, 10, 9, 8] (4, 9) [8.5] `shouldBe` True
        describe "abrupts"$ do
                it "only accepts, shortens or rejects candidate nadirs without introducing new ones" $ do
                        forAll $ do
                                pressures <- sized $ \size-> listOf arbitrary >$ take size
                                candidates <- resize (length pressures) randomNadirs :: Gen [(Index, Count)]
                                return (pressures, candidates)
                       $ \(pressures,candidates)-> length (abrupts 3 pressures candidates) `shouldSatisfy` (<= length candidates)
                it "picks up a decrescendo starting at the beginning" $ do
                        abrupts 3 ([4,3..1] ++ [5]) [(0, 3)] `shouldBe` [(0, 3)]
                it "dismisses a decrescendo without reversal at the very end" $ do
                        abrupts 3 [4,3..1] [(0, 3)] `shouldBe` []
                it "dismisses a nadir not followed by an abrupt return to baseline" $ do
                        let before =
                                [5.978584292334780670e-02 ,5.812644961202821647e-02 ,5.604743960129447700e-02
                                ,5.486488344840005638e-02 ,5.304336550321590849e-02 ,5.187034609348999126e-02]
                        let between =
                                [5.231857302240965069e-02 ,5.144119265090733861e-02 ,5.067825319742706031e-02
                                ,5.106925966733570632e-02 ,5.156517031209788271e-02 ,4.953384401720665325e-02
                                ,5.074501039960658405e-02 ,5.102157595149318242e-02 ,5.290031435568835760e-02
                                ,5.185127260715298447e-02 ,5.189895632299550143e-02 ,5.022048952533889749e-02
                                ,5.095481874931365868e-02 ,5.118370058535774703e-02 ,5.116462709902074024e-02
                                ,5.143165590773883522e-02 ,5.281448366717182707e-02 ,5.258560183112774566e-02
                                ,5.122184755803176059e-02 ,5.290985109885686100e-02 ,5.065917971109005352e-02
                                ,4.960060121938617700e-02 ,5.199432375468053535e-02 ,5.162239077110890306e-02
                                ,5.004882814830583643e-02 ,4.948616030136413629e-02]
                        let descent = [5.199432375468053535e-02 ,5.162239077110890306e-02
                                      ,5.004882814830583643e-02 ,4.948616030136413629e-02]
                        let after = [4.953384401720665325e-02]
                        abrupts 3 (between ++ descent ++ after) [(length between - 1, length descent)] `shouldBe` []
                        abrupts 3 (before ++ between ++ descent ++ after) [(length between + length before - 1, length descent)] `shouldBe` []
        describe "liftA2 (<*>) abrupts declinesLongerThan 3" $ do
                let search = liftA2 (<*>) abrupts declinesLongerThan 3
                it "rejects a decline terminating the analysis period" $ do
                  let sawtooth4 = [4,3..1]
                  search sawtooth4 `shouldBe` []
                  let sawtooth5 = [5,4..1]
                  search sawtooth5 `shouldBe` []
                it "accepts a decrescendo followed by a jump to above the starting pressure" $ do
                  let sawtooth45 = [4,3..1] ++ [5]
                  search sawtooth45 `shouldBe` [(0, 3)]
                  let sawtooth56 = [5,4..1] ++ [6]
                  search sawtooth56 `shouldBe` [(0, 4)]
                it "finds baselines and short decrescendos in a saw blade" $ do
                  let sawtooth = [5,4..1]
                  let saw = replicate 8 sawtooth & concat
                  search saw `shouldBe` take 7 (iterate (\(index,_)-> (index+5, 3)) (1,3))
                it "finds and accepts every crescendo in the mandible of a mountain lion" $ do
                  let tooth = [5,4..1] ++ [6,3]
                  let mandible = replicate 8 tooth & concat
                  search mandible `shouldBe` take 8 (iterate (\(index,_)-> (index+7, 4)) (0, 4))
        describe "decrescendosFulfilling" $ do
          it "ignores decrescendos not terminated by abrupt reversal" $ do
            (>$) vsn007 (decrescendosFulfilling [belowBaseline, abrupt] 5) `shouldReturn` [(22,5)]
--       describe "variance" $ do
--         it "works on a few examples" $ examples variance
--           [([1..9], 20/3)]

