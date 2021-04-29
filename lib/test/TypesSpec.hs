module TypesSpec ( spec ) where

import Data.Function ( (&) )
import Data.Functor( (<&>) )
import Data.List( sort )
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
    describe "samtelja" $ do
      it "preserves length" $ do
        forAll (listOf arbitraryDateTimes) $ \periodLists->
          length (samtelja periodLists) >= (map length periodLists & sum)
      it "counts each period at least once" $ do
        forAll (listOf arbitraryDateTimes) $ \periodLists->
          sum (samtelja periodLists) >= (map length periodLists & sum)
    describe "telja" $ do
      it "preserves the length of the flattened list of datetimes" $ do
        forAll (listOf1 arbitraryDateTimes) $ \periodLists->
          forAll (elements periodLists) $ \periodList-> do
            let flattened = flattenAll periodLists
            length(telja flattened periodList) `shouldBe` length flattened
    describe "mergeAll" $ do
      it "returns an ascending list if each input is ascending" $ do
        forAll (arbitrary ::Gen [SortedList DateTime]) $ \wrapped-> do
          let lists = map getSorted wrapped
          mergeAll lists `shouldBe` (concat lists & sort)


nubSort:: [DateTime]-> [DateTime]
nubSort = sort <&> fastnub
    where fastnub(one:other:rest)= if one==other then fastnub(one:rest) else one:fastnub(other:rest)
          fastnub(short)= short

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
