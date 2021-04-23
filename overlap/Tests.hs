{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests where

import Control.Exception( ErrorCall, catch, evaluate )
import Control.Monad( replicateM )
import Data.Function( (&) )
import Data.IntervalSet( empty, fromList )
import Data.List( sort )
import Data.Ratio( Ratio )
import Prelude hiding( readFile )
import Test.Hspec
import Test.Hspec.Core.QuickCheck( modifyMaxSuccess )
import Text.ParserCombinators.ReadP( eof, ReadP, readP_to_S )
import Test.QuickCheck (arbitrary, getPositive, Positive, Gen, choose, property)

import Bjartur.Time

import Main ( (>$), coefficient, jaccard, overlaps, union, measures )

examples:: (Show a, Show b, Eq b)=> (a -> b)-> [(a,b)]-> Expectation
examples f= (f >$ shouldBe) & uncurry & mapM_

parse:: ReadP output-> String-> output
parse parser= readP_to_S (parser <* eof)
  >$ last
  >$ fst

fromTuples:: [(DateTime, DateTime)]-> Intervals
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
main= hspec.modifyMaxSuccess(10*) $ do
    describe "measure" $ do
      it "can calculate the length of an period starting up to 59 whole minutes after midnight and ending that same night, or the day or evening after"
       . property $ do
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
        return (jaccard disjoints disjoints `shouldBe` 1)
      it "considers no non-empty set 100% correlated with the empty set"
       . property $ do
        disjoints<- arbitrarySet
        return $ do
          jaccard disjoints empty `shouldBe` 0
          jaccard empty disjoints `shouldBe` 0
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


nubSort:: [DateTime]-> [DateTime]
nubSort = sort >$ fastnub
    where fastnub(one:other:rest)= if one==other then fastnub(one:rest) else one:fastnub(other:rest)
          fastnub(short)= short

arbitrarySet:: Gen Intervals
arbitrarySet= do
      size<- arbitrary:: Gen Int
      datetimes<- replicateM (2 * abs size + 2) arbitrary >$ nubSort ::Gen[DateTime]
      let pair[]= []
          pair[_]= undefined
          pair(a:b:xs)= period a b : pair xs
      let disjoints= pair datetimes
      return $! fromList disjoints
