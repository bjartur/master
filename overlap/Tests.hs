{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests where

import Control.Exception( ErrorCall, catch, evaluate )
import Control.Monad( replicateM )
import Data.Function( (&) )
import Data.List( intercalate, sort )
import Prelude hiding( readFile )
import Test.Hspec
import Test.Hspec.Core.QuickCheck( modifyMaxSuccess )
import Text.ParserCombinators.ReadP( eof, ReadP, readP_to_S )
import Test.QuickCheck (Arbitrary, arbitrary, Gen, choose, property)

import Main ( DateTime(..), couple, correlation, dateTime, digit, file, Interval, measure, overlap, overlaps, union, row, (>$) )

examples:: (Show a, Show b, Eq b)=> (a -> b)-> [(a,b)]-> Expectation
examples f= (f >$ shouldBe) & uncurry & mapM_

parse:: ReadP output-> String-> output
parse parser= readP_to_S (parser <* eof)
  >$ last
  >$ fst

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
    describe "digit" $ do
        it "parses a digit" $ do
            examples (parse digit) $
                zip (map return ['0'..'9']) [0..9]
        it "rejects 0x as not a digit" $ do
            readP_to_S (digit>>eof) "0x" `shouldBe` []
    describe "couple" $ do
        it "parses a couple of digits" $ do
            examples (parse couple) $
                  zip (replicate 2 ['0'..'9'] & sequence) [0..99]
        it "rejects 0x as not a couple of digits" $ do
            shouldNotAccept (couple>>eof) "0x"
    describe "dateTime" $ do
        it "parses actual dates and a timestamps correctly" $ do
          examples (parse dateTime) [
            ("2014-11-12 03:10:31.000000", DateTime 2014 11 12 3 10 31),
            ("2014-11-12 01:44:40.000000", DateTime 2014 11 12 1 44 40)
            ]
        it "does not reject generated, valid dates and timestamps" $ do
          property $ do
            years<- pick 1 9999 >$ zeropad 4
            months<- pick 10 12 >$ zeropad 2
            days<- pick 10 28 >$ zeropad 2
            hours<- pick 10 23 >$ zeropad 2
            minutes<- pick 10 59 >$ zeropad 2
            seconds<- pick 10 59 >$ zeropad 2
            microseconds<- pick 1 999999 >$ zeropad 6
            pure (years ++ "-" ++ months ++ "-" ++ days ++ " " ++ hours ++ ":" ++ minutes ++ ":" ++ seconds ++ "." ++ microseconds)
             >$ parse dateTime
             >$ \(DateTime _ _ _ _ _ _) -> True
        it "rejects timestamps containing letters" $ do
          property $ do
            years<- pick 1 9999 >$ zeropad 4
            months<- pick 1 12 >$ zeropad 2
            days<- pick 1 28 >$ zeropad 2
            hours<- pick 1 23 >$ zeropad 2
            minutes<- choose('a','z') & replicate 2 & sequence
            seconds<- pick 1 59 >$ zeropad 2
            microseconds<- pick 1 999999 >$ zeropad 6
            pure (years ++ "-" ++ months ++ "-" ++ days ++ " " ++ hours ++ ":" ++ minutes ++ ":" ++ seconds ++ "." ++ microseconds)
             >$ shouldNotAccept dateTime

    describe "row" $ do
      it "parses score CSV rows" $ do
          examples (parse row) [
            ("2014-11-12 01:12:35.917000,2014-11-12 01:12:45.036000\n", (DateTime 2014 11 12 1 12 36, DateTime 2014 11 12 1 12 45))
           ,("2015-01-21 22:43:25.000000,2015-01-21 22:43:40.000000\n", (DateTime 2015 1 21 22 43 25, DateTime 2015 1 21 22 43 40))
           ,("2015-01-21 22:39:33.125000,2015-01-21 22:39:44.404000\n", (DateTime 2015 1 21 22 39 33, DateTime 2015 1 21 22 39 44))
            ]
    describe "file" $ do
      it "accepts several autoscore rows as valid" $
        parse file (intercalate "\n" [
          "2014-11-12 01:16:34.941000,2014-11-12 01:16:45.300000",
          "2014-11-12 01:17:03.899000,2014-11-12 01:17:14.458000",
          "2014-11-12 01:20:56.804000,2014-11-12 01:21:10.483000",
          "2014-11-12 01:21:52.720000,2014-11-12 01:22:01.919000",
          "2014-11-12 01:22:27.158000,2014-11-12 01:22:36.797000",
          "2014-11-12 01:24:26.790000,2014-11-12 01:24:37.749000",
          "2014-11-12 01:25:35.385000,2014-11-12 01:25:53.424000",
          "2014-11-12 01:26:24.142000,2014-11-12 01:26:38.061000",
          "2014-11-12 01:26:41.941000,2014-11-12 01:26:52.860000",
          "2014-11-12 01:27:40.017000,2014-11-12 01:27:50.576000",
          "2014-11-12 01:29:37.569000,2014-11-12 01:29:50.768000",
        ""])
        `shouldStartWith` [(DateTime 2014 11 12 1 16 35, DateTime 2014 11 12 1 16 45),
                             (DateTime 2014 11 12 1 17 4, DateTime 2014 11 12 1 17 14)]
      it "accepts several manually exported as valid (after conversion)" $
        parse file (intercalate "\n" [
          "2015-01-21 22:51:45.000000,2015-01-21 22:51:56.000000",
          "2015-01-21 22:52:49.000000,2015-01-21 22:53:09.000000",
          "2015-01-21 22:53:20.000000,2015-01-21 22:53:39.000000",
          "2015-01-21 22:53:46.000000,2015-01-21 22:54:16.000000",
          "2015-01-21 22:54:23.000000,2015-01-21 22:54:39.000000",
          "2015-01-21 22:54:45.000000,2015-01-21 22:55:19.000000",
          "2015-01-21 22:56:10.000000,2015-01-21 22:56:35.000000",
          "2015-01-21 22:56:46.000000,2015-01-21 22:57:03.000000",
          "2015-01-21 22:57:21.000000,2015-01-21 22:57:43.000000",
          "2015-01-21 22:57:55.000000,2015-01-21 22:58:12.000000",
          "2015-01-21 22:58:35.000000,2015-01-21 22:58:52.000000",
          "2015-01-21 22:59:49.000000,2015-01-21 23:00:02.000000",
          "2015-01-21 23:00:05.000000,2015-01-21 23:00:16.000000",
          "2015-01-21 23:00:20.000000,2015-01-21 23:00:31.000000",
          "2015-01-21 23:00:35.000000,2015-01-21 23:01:01.000000",
          "2015-01-21 23:01:06.000000,2015-01-21 23:01:19.000000",
          "2015-01-21 23:01:41.000000,2015-01-21 23:02:20.000000",
          "2015-01-21 23:02:24.000000,2015-01-21 23:02:39.000000",
          "2015-01-21 23:02:44.000000,2015-01-21 23:02:55.000000",
          "2015-01-21 23:03:01.000000,2015-01-21 23:03:34.000000",
        ""])
        `shouldEndWith` [(DateTime 2015 01 21 23 02 44, DateTime 2015 01 21 23 02 55),
                      (DateTime 2015 01 21 23 03 01, DateTime 2015 01 21 23 03 34)]
    let a `minus` b= DateTime (year a - year b) (month a - month b) (day a - day b) (hour a - hour b) (minute a - minute b) (second a - second b)
    describe "measure" $ do
      it "can calculate the length of an interval starting up to 59 whole minutes after midnight and ending that same night, or the day or evening after"
       . property $ do
        end<- arbitrary:: Gen DateTime
        let beginning= end { hour = 0, second = 0 }
        return $ measure (beginning, end) `shouldBe` 3600*(hour end) + second end
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
    describe "overlaps" . it "is invariant under a superset" . property $ do
      size<- arbitrary:: Gen Int
      datetimes<- replicateM (2 * abs size + 2) arbitrary >$ nubSort:: Gen [DateTime]
      let overarching= (head datetimes, last datetimes)
      let pair[]= []
          pair[_]= undefined
          pair(a:b:xs)= (a,b) : pair xs
      let disjoints= pair datetimes
      return $ do
        overlaps disjoints [overarching] `shouldBe` overlaps disjoints disjoints
        overlaps disjoints disjoints `shouldNotBe` 0
    describe "overlap" $ do
      it "calculates the number of seconds for which a couple of intervals overlap" $
        examples (overlap (DateTime 1 2 3 4 5 6, DateTime 1 2 3 4 7 9)) [
        ( (DateTime 1 2 3 4 6 6, DateTime 1 2 3 4 10 6), 63 ),
            ( (DateTime 1 2 3 4 6 6, DateTime 1 2 3 4 7 6), 60 )
        ]
    describe "overlaps" $ do
      it "can measure the intersection of, one one hand, a sequence of disjoint intervals and, on the other, an interval containing them all"
       . property $ do
        (overarching,disjoints)<- arbitrarySet
        return (overlaps [overarching] disjoints `shouldBe` sum(map measure disjoints))
    describe "union" $ do
      it "of a set and itself should be the set unchanged"
       . property $ do
        (_,disjoints)<- arbitrarySet
        return (union disjoints disjoints `shouldBe` disjoints)
    describe "correlation" $ do
      it "considers a list equivalent to itself"
       . property $ do
        (_,disjoints)<- arbitrarySet
        return (correlation disjoints disjoints `shouldBe` 1)
      it "considers the empty set 100% correlated with itself" $
        correlation [] [] `shouldBe` 1
      it "considers no non-empty set 100% correlated with the empty set"
       . property $ do
        (_,disjoints)<- arbitrarySet
        return $ do
          correlation disjoints [] `shouldNotBe` 1
          correlation [] disjoints `shouldNotBe` 1
    describe "unions" $ do
      it "two 10s intervals that overlap 1s measures a total of 19s" $ do
        let intervalA = ((DateTime 2020 07 09 15 56 00), (DateTime 2020 07 09 15 56 10))
            intervalB = ((DateTime 2020 07 09 15 56 09), (DateTime 2020 07 09 15 56 19))
            setUnion = union [intervalA] [intervalB]
        (map measure setUnion & sum) `shouldBe` 19
      it "unions should be disjoint" $ do
        let setA = [ ((DateTime 2020 07 09 15 56 00), (DateTime 2020 07 09 15 56 10)) ]
            setB = [ ((DateTime 2020 07 09 15 30 09), (DateTime 2020 07 09 15 30 19))
                   , ((DateTime 2020 07 09 15 56 00), (DateTime 2020 07 09 15 56 10)) ]
            setUnion = union setA setB
        (map measure setUnion & sum) `shouldBe` 20

instance Arbitrary DateTime where
  arbitrary= pure DateTime
     <*> choose(1,9999)
     <*> choose(1,12)
     <*> choose(1,28)
     <*> choose(1,24)
     <*> choose(1,60)
     <*> choose(1,60)

nubSort:: [DateTime]-> [DateTime]
nubSort = sort >$ fastnub
    where fastnub(one:other:rest)= if one==other then fastnub(one:rest) else one:fastnub(other:rest)
          fastnub(short)= short

arbitrarySet:: Gen (Interval,[Interval])
arbitrarySet= do
      size<- arbitrary:: Gen Int
      datetimes<- replicateM (2 * abs size + 2) arbitrary >$ nubSort:: Gen [DateTime]
      let overarching= (head datetimes, last datetimes)
      let pair[]= []
          pair[_]= undefined
          pair(a:b:xs)= (a,b) : pair xs
      let disjoints= pair datetimes
      return (overarching,disjoints)
