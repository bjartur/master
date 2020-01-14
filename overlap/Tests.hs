{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests where

import Data.Function( (&) )
import Data.List
import Prelude hiding( readFile )
import Test.Hspec
import Text.ParserCombinators.ReadP( eof, ReadP, readP_to_S )
import Test.QuickCheck (Arbitrary, arbitrary, Gen, choose, property)

import Main ( DateTime(..), couple, dateTime, diff, digit, file, interval, row, (>$) )

examples:: (Show a, Show b, Eq b)=> (a -> b)-> [(a,b)]-> Expectation
examples f= mapM_(uncurry (f >$ shouldBe))

parse:: ReadP output-> String-> output
parse parser= readP_to_S (parser <* eof)
  >$ last
  >$ fst

pick:: Int-> Int-> Gen String
pick lower upper= choose(lower,upper) >$ show

zeropad:: Int-> String-> String
zeropad width input= if length(take width input) < width then '0':input & zeropad width else input

main:: IO ()
main= hspec $ do
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
            readP_to_S (couple>>eof) "0x" `shouldBe` []
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
             >$ readP_to_S dateTime
             >$ (==[])

    describe "row" $ do
      it "parses a score CSV row" $ do
          examples (parse row) [
            ("2014-11-12 01:12:35.917000,2014-11-12 01:12:45.036000\r\n", (DateTime 2014 11 12 1 12 36, DateTime 2014 11 12 1 12 45))
            ]
    describe "file" $ do
      it "accepts several actual rows as valid" $
        parse file (intercalate "\r\n" [
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
          `shouldStartWith` [(DateTime 2014 11 12 1 16 35, DateTime 2014 11 12 1 16 45), (DateTime 2014 11 12 1 17 4, DateTime 2014 11 12 1 17 14)]

    let a `minus` b= DateTime (year a - year b) (month a - month b) (day a - day b) (hour a - hour b) (minute a - minute b) (second a - second b)
    describe "interval" $ do
      it "can calculate the length of an interval starting up to 59 whole minutes after midnight and ending that same night, or the day or evening after" . property $ do
        end<- arbitrary :: Gen DateTime
        let beginning= end { hour = 0, second = 0 }
        return $ interval (beginning, end) `shouldBe` 3600*(hour end) + second end
    describe "instance DateTime Ord" $ do
      it "considers dates farther from year 1 to be greater" $ do
        let a `lessThan` b= a `minus` b & \datetime-> dropWhile (\accessor-> accessor datetime == 0) [year, day, hour, minute, second] & \accessors-> if null accessors then False else head accessors datetime < 0
        property (\a b-> (a < b) == (a `lessThan` b))
    describe "diff" $ do
      it "is defined" $
        diff `shouldNotBe` ()

instance Arbitrary DateTime where
  arbitrary= pure DateTime
     <*> choose(1,9999)
     <*> choose(1,12)
     <*> choose(1,28)
     <*> choose(1,24)
     <*> choose(1,60)
     <*> choose(1,60)
