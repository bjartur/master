{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests where

import Control.Applicative( liftA2 )
import Control.Exception( ErrorCall, catch, evaluate )
import Control.Lens.Combinators( _2, both, mapped, over )
import Control.Monad( ap, replicateM )
import Data.Function( (&), on )
import Data.Functor( (<&>) )
import Data.IntervalSet( empty, fromList )
import Data.List( sort, sortOn )
import Data.Ratio( Ratio )
import Prelude hiding( readFile )
import Test.Hspec
import Test.Hspec.QuickCheck( modifyMaxSuccess, prop )
import Text.ParserCombinators.ReadP( eof, ReadP, readP_to_S )
import Test.QuickCheck( (===), (==>), Gen, arbitrary, choose, elements, forAll, getSize, listOf, listOf1, Positive(getPositive), property, vectorOf, SortedList(getSorted) )

import Bjartur.Time( DateTime(..), Events, measure, measure', nubSort, period )
import Histogram

import Main ( (>$), coefficient, jaccard, overlaps, union, measures )

examples:: (Show a, Show b, Eq b)=> (a -> b)-> [(a,b)]-> Expectation
examples f= (f >$ shouldBe) & uncurry & mapM_

parse:: ReadP output-> String-> output
parse parser= readP_to_S (parser <* eof)
  >$ last
  >$ fst

fromTuples:: [(DateTime, DateTime)]-> Events
fromTuples= fromList . map (uncurry period)

-- pick a string of decimal digits representing a number in the given inclusive range.
-- | @pick lower upper@
--
--   [@lower@] The lowest number to choose from.
--
--   [@upper@] The highest number to choose from.
pick:: Int-> Int-> Gen String
pick lower upper= choose(lower,upper) >$ show

-- @zeropad width input@
-- Left-pad the @String@ @input@ with zeros until it's no longer shorter than @width@.
zeropad:: Int-> String-> String
zeropad width input= if length(take width input) < width then '0':input & zeropad width else input

shouldNotAccept:: (Show a, Eq a)=> ReadP a-> String-> Expectation
shouldNotAccept parser input=
  catch (evaluate $ readP_to_S parser input) (\exception-> (exception:: ErrorCall) `seq` return []) `shouldReturn` []

main:: IO ()
main= hspec $ do
  modifyMaxSuccess(*10) $ do
    describe "measure" $ do
      prop "can calculate the length of an period starting up to 59 whole minutes after midnight and ending that same night, or the day or evening after" $ do
        end<- arbitrary ::Gen DateTime
        let beginning= end { hour = 0, second = 0 }
        return $ measure (period beginning end) `shouldBe` 3600*(hour end) + second end
    describe "overlap coefficient" $ do
      it "comparing a list with itself is 100%" $
        coefficient 0 1 0 `shouldBe` 1
      it "disjoint sets should have 0% correlation"
       . property $ do
        n<- arbitrary ::Gen (Positive(Ratio Int))
        let n' = getPositive n
        m<- arbitrary ::Gen (Positive(Ratio Int))
        let m' = getPositive m
        let left = n' / (n' + m')
        let right = 1 - left
        return $ do
          coefficient left 0 right `shouldBe` 0
      it "of 0.3 (0.4) 0.3 is 4/7" $
          coefficient 0.3 (0.4) 0.3 `shouldBe` 0.4/(0.3+0.4)
    describe "jaccard" $ do
      it "considers a list equivalent to itself"
       . property $ do
        disjoints<- arbitrarySet
        return(0 < measures disjoints ==> jaccard disjoints disjoints `shouldBe` 1)
      it "considers no non-empty set 100% correlated with the empty set"
       . property $ do
        disjoints<- arbitrarySet
        return $ 0 < measures disjoints ==> (do
          jaccard disjoints empty `shouldBe` 0
          jaccard empty disjoints `shouldBe` 0)
  describe "overlaps" $ do
    it "identical interval sets overlap totally" $ do
      let setA = fromList [period (DateTime 2020 07 09 15 56 00) (DateTime 2020 07 09 15 56 10)]
          setB = fromList [period (DateTime 2020 07 09 15 56 00) (DateTime 2020 07 09 15 56 10)]
      overlaps setA setB `shouldBe` 10
    it "partially overlapping intervals" $ do
      let setA = fromList [period (DateTime 2020 07 09 15 56 00) (DateTime 2020 07 09 15 56 15)]
          setB = fromList [period (DateTime 2020 07 09 15 56 00) (DateTime 2020 07 09 15 56 10)]
      overlaps setA setB `shouldBe` 10
    it "non-overlapping sets should return 0 seconds overlap" $ do
      let setA = fromList [period (DateTime 2020 07 09 15 56 00) (DateTime 2020 07 09 15 56 10)]
          setB = fromList [period (DateTime 2020 07 09 15 56 10) (DateTime 2020 07 09 15 56 20)]
      overlaps setA setB `shouldBe` 0
    it "1 sec overlapping sets" $ do
      let setA = fromList [period (DateTime 2020 07 09 15 56 00) (DateTime 2020 07 09 15 56 10)]
          setB = fromList [period (DateTime 2020 07 09 15 56 09) (DateTime 2020 07 09 15 56 20)]
      overlaps setA setB `shouldBe` 1
  describe "unions" $ do
    it "two 10s intervals that overlap 1s measures a total of 19s" $ do
      let setA = fromList [period (DateTime 2020 07 09 15 56 00) (DateTime 2020 07 09 15 56 10)]
          setB = fromList [period (DateTime 2020 07 09 15 56 09) (DateTime 2020 07 09 15 56 19)]
          setUnion = union setA setB
      measures setUnion `shouldBe` 19
  describe "count" $ do
    it "preserves length" $ do
      forAll (listOf1 arbitraryDateTimes) $ \periodLists->
        length (ap count flattenAll $ periodLists) >= (map length periodLists & sum)
    it "counts each period at least once" $ do
      forAll (listOf arbitraryDateTimes) $ \periodLists->
        (ap count flattenAll periodLists & sum) >= (map length periodLists & sum)
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
      let expected = [("0", 1),("1", 4),("2", 1)]
      (discreteHistogram listOfIntervals & sortOn fst) `shouldBe` expected
    prop "is compatible with its original implementation" $ do
      periodList <- listOf1 arbitraryDateTimes
      yr <- arbitrary
      mo <- choose(1,12)
      over (mapped.mapped.both) (fixMonth yr mo) periodList
        & liftA2 (on shouldBe sort) discreteHistogram discreteHistogram'
        & return
  describe "treesum" $ do
    prop "is compatible with its original implementation" $
      liftA2 (on (===) sort) treesum (sort <&> group <&> over (mapped._2) sum )


-- input: list of pairs
-- output: same list of pairs after merging adjacent pairs with the same fst item
-- the snd items are collected into a list
group :: Eq a=> [(a, b)]-> [(a, [b])]
group [] = []
group ((firstCount,event):most) = do
          let (matches, rest) = span ((firstCount ==) .fst) ((firstCount, event):most)
          (firstCount, map snd matches) : group rest

-- input: one recording: a list that contains a list of events for each pattern
-- output: [(show the number patterns matching sleep simultaneously, the total duration of sleep matching so many patterns)]
discreteHistogram' :: [[(DateTime,DateTime)]]-> [(String,Int)]
discreteHistogram' eventsByPattern = do
          let countedEvents = flatCount eventsByPattern & sortOn fst
          (eachCount, correspondingEvents) <- group countedEvents
          [(show eachCount, map measure' correspondingEvents & sum)]

seconds :: Int-> DateTime
seconds n = DateTime 0 0 0 0 0 n

toDateTimes :: [[(Int,Int)]]-> [[(DateTime,DateTime)]]
toDateTimes = map (map (\(n,m)-> (n & seconds, m & seconds)))

fixMonth:: Int-> Int-> DateTime-> DateTime
fixMonth yr mo datetime= datetime{month = mo, year = yr }

arbitraryDateTimes:: Gen [(DateTime,DateTime)]
arbitraryDateTimes= do
      size<- getSize
      mo <- arbitrary ::Gen Int
      yr <- arbitrary ::Gen Int
      datetimes<- vectorOf (2 * abs size + 2) arbitrary
        <&> map (fixMonth yr mo)
        <&> nubSort ::Gen[DateTime]
      let pair[]= []
          pair[_]= []
          pair(a:b:xs)= (a, b) : pair xs
      let disjoints= pair datetimes
      return $! disjoints

arbitrarySet:: Gen Events
arbitrarySet= do
      size<- arbitrary:: Gen Int
      mo <- arbitrary ::Gen Int
      yr <- arbitrary ::Gen Int
      datetimes<- replicateM (2 * abs size + 2) arbitrary
        >$ map (\datetime-> datetime{month = mo, year = yr })
        >$ nubSort ::Gen[DateTime]
      let pair[]= []
          pair[_]= []
          pair(a:b:xs)= period a b : pair xs
      let disjoints= pair datetimes
      return $! fromList disjoints
