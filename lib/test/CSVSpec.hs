module CSVSpec ( spec ) where

import Control.Exception( ErrorCall, catch, evaluate )
import Data.Function( (&) )
import Data.IntervalSet( fromList, isSubsetOf )
import Data.List( intercalate )
import Prelude hiding( readFile )
import Test.Hspec
import Test.Hspec.Core.QuickCheck( modifyMaxSuccess )
import Text.ParserCombinators.ReadP( eof, ReadP, readP_to_S )
import Test.QuickCheck (Gen, choose, property)

import Bjartur.Time
import Bjartur.CSV ( couple, dateTime, digit, file, row, (>$) )

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

spec :: Spec
spec = modifyMaxSuccess(10*) $ do
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
            ("2014-11-12 01:12:35.917000,2014-11-12 01:12:45.036000\n", uncurry period (DateTime 2014 11 12 1 12 36, DateTime 2014 11 12 1 12 45))
           ,("2015-01-21 22:43:25.000000,2015-01-21 22:43:40.000000\n", uncurry period (DateTime 2015 1 21 22 43 25, DateTime 2015 1 21 22 43 40))
           ,("2015-01-21 22:39:33.125000,2015-01-21 22:39:44.404000\n", uncurry period (DateTime 2015 1 21 22 39 33, DateTime 2015 1 21 22 39 44))
            ]
    describe "file" $ do
      let subset = isSubsetOf . fromTuples
      it "accepts several autoscore rows as valid" $
        subset [(DateTime 2014 11 12 1 16 35, DateTime 2014 11 12 1 16 45),
                             (DateTime 2014 11 12 1 17 4, DateTime 2014 11 12 1 17 14)]
        (parse file $ intercalate "\n" [
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
      it "accepts several manually exported as valid (after conversion)" $
        subset [(DateTime 2015 01 21 23 02 44, DateTime 2015 01 21 23 02 55),
                      (DateTime 2015 01 21 23 03 01, DateTime 2015 01 21 23 03 34)]
        (parse file $ intercalate "\n" [
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
