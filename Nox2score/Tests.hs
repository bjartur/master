{-# LANGUAGE ApplicativeDo #-}
module Tests where

import Control.Applicative (liftA2)
import Data.Function
import Data.List
import Prelude hiding (readFile)
import Test.Hspec
import Text.ParserCombinators.ReadP (eof, ReadP, readP_to_S)
import Test.QuickCheck (choose, property)

import Main ( DateTime(..), couple, dateTime, digit, file, isRecordingName, row, (>$) )

finally:: ReadP a -> ReadP a
finally parser= do
  value <- parser
  eof
  return value

examples:: (Show a, Show b, Eq b)=> (a -> b)-> [(a,b)]-> Expectation
examples f= mapM_(uncurry (f >$ shouldBe))

parse:: ReadP output-> String-> output
parse parser= readP_to_S (finally parser)
  >$ last
  >$ fst

two:: Applicative possibilities=> possibilities Char-> possibilities String
two possibilities= do
  tens <-possibilities
  singles <-possibilities
  pure [tens, singles]

main:: IO ()
main= hspec $ do
    describe "digit" $ do
        it "parses a digit" $ do
            examples (parse digit) $
                zip (map return ['0'..'9']) [0..9]
        it "rejects 0x as not a digit" $ do
            readP_to_S (digit>>eof) "0x" `shouldBe` []
    describe "couple" $ do
        it "parses two digits" $ do
            examples (parse couple) $
                  zip (two ['0'..'9']) [0..99]
        it "rejects 0x as not two digits" $ do
            readP_to_S (couple>>eof) "0x" `shouldBe` []
    describe "dateTime" $ do
        it "parses actual dates and a timestamps correctly" $ do
          examples (parse dateTime) [
            ("12/11/2014 03:10:31,", DateTime 2014 11 12 3 10 31),
            ("12/11/2014 01:44:40,", DateTime 2014 11 12 1 44 40)
            ]
        it "does not reject generated, valid dates and timestamps" $ do
          property $ do
            day <-two (choose ('0','9'))
            month <-two (choose ('0','9'))
            year <-liftA2 (++) (two (choose ('0','9'))) (two (choose ('0','9')))
            hour <-two (choose ('0','9'))
            minute <-two (choose ('0','9'))
            second <-two (choose ('0','9'))
            pure (day ++ "/" ++ month ++ "/" ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second ++ ",")
             >$ parse dateTime
             >$ \(DateTime _ _ _ _ _ _) -> True
        it "rejects timestamps containing letters" $ do
          property $ do
            day <-two (choose ('0','9'))
            month <-two (choose ('0','9'))
            year <-liftA2 (++) (two (choose ('0','9'))) (two (choose ('0','9')))
            hour <-two (choose ('a','z'))
            minute <-two (choose ('0','9'))
            second <-two (choose ('0','9'))
            pure (day ++ "/" ++ month ++ "/" ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second ++ ",")
             >$ readP_to_S dateTime
             >$ (==[])
    describe "row" $ do
        it "parses a marker CSV row (ignoring sleep stage if present)" $ do
          examples (parse row) [
            ("12/11/2014 01:44:40,12/11/2014 01:44:54,,\r\n", (DateTime 2014 11 12 1 44 40, DateTime 2014 11 12 1 44 54)),
            ("13/07/2011 24:43:40,14/07/2011 00:00:04,Léttur Svefn,\r\n", (DateTime 2011 7 13 24 43 40, DateTime 2011 7 14 0 0 4))
            ]
    describe "file" $ do
      it "accepts several actual rows as valid" $
        parse file (intercalate "\r\n" ["Start Time,End Time,Sleep,",
          "[],[],[],",
          "29/12/2014 03:16:17,29/12/2014 03:16:44,REM,",
          "29/12/2014 03:16:48,29/12/2014 03:17:08,REM,",
          "29/12/2014 03:17:53,29/12/2014 03:18:12,REM,",
          "29/12/2014 03:18:25,29/12/2014 03:18:42,REM,",
          "29/12/2014 03:19:08,29/12/2014 03:19:30,REM,",
          "29/12/2014 03:19:43,29/12/2014 03:20:00,REM,",
          "29/12/2014 03:20:53,29/12/2014 03:21:05,REM,",
          "29/12/2014 03:21:12,29/12/2014 03:21:23,REM,",
          "29/12/2014 03:22:12,29/12/2014 03:22:52,REM,",
          "29/12/2014 03:23:06,29/12/2014 03:23:22,REM,",
          "29/12/2014 03:23:38,29/12/2014 03:23:49,REM,",
          "29/12/2014 03:37:48,29/12/2014 03:38:08,Léttur Svefn,",
          "29/12/2014 04:04:45,29/12/2014 04:05:11,,",
          "29/12/2014 04:07:10,29/12/2014 04:07:37,,",
          ""])
          `shouldStartWith` [(DateTime 2014 12 29 3 16 17, DateTime 2014 12 29 3 16 44), (DateTime 2014 12 29 3 16 48, DateTime 2014 12 29 3 17 8)]
    it "accepts the contents of an actual file as valid" $
      parse file (intercalate "\r\n" [
        "Start Time,End Time,Sleep,",
        "[],[],[],",
        "02/12/2014 00:40:00,02/12/2014 00:40:27,,",
        "02/12/2014 02:57:55,02/12/2014 02:58:14,REM,",
        "02/12/2014 02:58:18,02/12/2014 02:58:39,REM,",
        "02/12/2014 02:58:40,02/12/2014 02:59:05,REM,",
        "02/12/2014 03:03:05,02/12/2014 03:04:00,REM,",
        "02/12/2014 03:04:18,02/12/2014 03:05:00,REM,",
        "02/12/2014 03:05:14,02/12/2014 03:05:33,REM,",
        "02/12/2014 03:05:41,02/12/2014 03:06:22,REM,",
        "02/12/2014 03:06:34,02/12/2014 03:06:47,REM,",
        "02/12/2014 03:06:59,02/12/2014 03:07:29,REM,",
        "02/12/2014 03:08:01,02/12/2014 03:08:17,REM,",
        "02/12/2014 03:09:29,02/12/2014 03:09:52,REM,",
        "02/12/2014 03:09:58,02/12/2014 03:10:21,REM,",
        "02/12/2014 03:10:47,02/12/2014 03:11:08,REM,",
        "02/12/2014 03:11:13,02/12/2014 03:11:38,REM,",
        "02/12/2014 03:11:46,02/12/2014 03:12:20,REM,",
        "02/12/2014 03:12:26,02/12/2014 03:12:54,REM,",
        "02/12/2014 03:12:59,02/12/2014 03:13:21,REM,",
        "02/12/2014 03:13:30,02/12/2014 03:13:53,REM,",
        "02/12/2014 03:14:01,02/12/2014 03:14:23,REM,",
        "02/12/2014 03:14:59,02/12/2014 03:15:19,REM,",
        "02/12/2014 03:15:21,02/12/2014 03:15:32,REM,",
        "02/12/2014 03:15:35,02/12/2014 03:15:51,REM,",
        "02/12/2014 03:16:04,02/12/2014 03:16:18,REM,",
        "02/12/2014 03:16:44,02/12/2014 03:17:17,REM,",
        "02/12/2014 03:17:28,02/12/2014 03:18:07,REM,",
        "02/12/2014 03:18:56,02/12/2014 03:19:30,REM,",
        "02/12/2014 03:20:27,02/12/2014 03:20:47,REM,",
        "02/12/2014 04:28:53,02/12/2014 04:29:15,Vaka,",
        ""
      ]) `shouldStartWith` [(DateTime 2014 12 2 0 40 0, DateTime 2014 12 2 0 40 27)]
    describe "recordingName" $ it "accepts recording names, if only, of the form VSN-14-080-0[0-2][3-9]" $ property $ do
      former <- choose('0','2')
      latter <- choose('3','9')
      pure $ isRecordingName ("VSN-14-080-0" ++ [former,latter])
