module TypesSpec ( spec ) where

import Data.Function ( (&) )
import Data.Functor( (<&>) )
import Data.List( sort, sortOn )
import Test.Hspec
import Test.QuickCheck ( Gen, SortedList(getSorted), arbitrary, elements, forAll, getSize, listOf, listOf1, property, vectorOf )

import Bjartur.Time

spec :: Spec
spec = do
    let a `minus` b= DateTime (year a - year b) (month a - month b) (day a - day b) (hour a - hour b) (minute a - minute b) (second a - second b)
    describe "instance DateTime Ord" $ do
      it "considers dates farther from year 1 to be greater" $ do
        let a `lessThan` b= a `minus` b
                          & \datetime->
                              dropWhile (\accessor-> accessor datetime == 0) [year, month, day, hour, minute, second]
                            & \accessors->
                                if null accessors
                                then False
                                else head accessors datetime < 0
        property $ \a b-> (a < b) == (a `lessThan` b)
    describe "count" $ do
      it "preserves length" $ do
        forAll (listOf1 arbitraryDateTimes) $ \periodLists->
          length (count periodLists (flattenAll periodLists)) >= (map length periodLists & sum)
      it "counts each period at least once" $ do
        forAll (listOf arbitraryDateTimes) $ \periodLists->
          sum (count periodLists (flattenAll periodLists)) >= (map length periodLists & sum)
    describe "labelFromOutside" $ do
      it "almost preserves the length of the flattened list of datetimes" $ do
        forAll (listOf1 arbitraryDateTimes) $ \periodLists->
          forAll (elements periodLists) $ \periodList-> do
            let flattened = flattenAll periodLists
            length(labelFromOutside flattened periodList) `shouldBe` length flattened - 1
      it "left-pads with False until the first period" $ do
        labelFromOutside [8,9,20,21::Int] [(20,21)] `shouldBe` [False,False,True]
    describe "mergeAll" $ do
      it "returns an ascending list if each input is ascending" $ do
        forAll (arbitrary ::Gen [SortedList DateTime]) $ \wrapped-> do
          let lists = map getSorted wrapped
          mergeAll lists `shouldBe` (concat lists & sort)
    describe "flattenAll" $ do
      it "can flatten a list of intervals which, in total, leave no gaps" $ do
        let listar = [[(3,5),(8,9)], [(4,7)], [(1,4)], [(4,9)]]
        flattenAll listar `shouldBe` [1::Int,3,4,5,7,8,9]
      it "works even if one interval is later then the others" $ do
        let listar = [[(3,5),(8,9)], [(4,7), (20,21)], [(1,4)], [(4,9)]]
        flattenAll listar `shouldBe` [1::Int,3,4,5,7,8,9,20,21]
    describe "count" $ do
      it "produces the count 0 if there's a gap" $ do
        let listar = [[(3,5),(8,9)], [(4,7), (20,21)], [(1,4)], [(4,9)]]
        count listar [1::Int,3,4,5,7,8,9,20,21] `shouldBe` [1,2,3,2,1,2,0,1]
    describe "discreteHistogram" $ do
      it "includes 0 as a label if there's a gap" $ do
        let listOfIntervals = toDateTimes [[(3,5),(8,9)], [(4,7)]]
        let expected = [(0, 1),(1, 4),(2, 1)]
        (discreteHistogram listOfIntervals & sortOn fst) `shouldBe` expected

seconds :: Int-> DateTime
seconds n = DateTime 0 0 0 0 0 n

toDateTimes :: [[(Int,Int)]]-> [[(DateTime,DateTime)]]
toDateTimes = map (map (\(n,m)-> (n & seconds, m & seconds)))

arbitraryDateTimes:: Gen [(DateTime,DateTime)]
arbitraryDateTimes= do
      size<- getSize
      mo <- arbitrary ::Gen Int
      yr <- arbitrary ::Gen Int
      datetimes<- vectorOf (2 * abs size + 2) arbitrary
        <&> map (\datetime-> datetime{month = mo, year = yr })
        <&> nubSort ::Gen[DateTime]
      let pair[]= []
          pair[_]= []
          pair(a:b:xs)= (a, b) : pair xs
      let disjoints= pair datetimes
      return $! disjoints
