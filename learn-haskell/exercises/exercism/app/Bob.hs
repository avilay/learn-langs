{-# LANGUAGE OverloadedStrings #-}

module Bob where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Char as C
import Test.HUnit


responseFor :: Text -> Text
responseFor text
  | cleanText == T.empty = "Fine. Be that way!"
  | isLoudQuestion cleanText = "Calm down, I know what I'm doing!"
  | isLoud cleanText = "Whoa, chill out!"
  | isQuestion cleanText = "Sure."
  | otherwise = "Whatever."

  where
    cleanText = T.strip text

    isQuestion phrase = T.last phrase == '?'

    isLoud phrase =
      let onlyLetters = T.filter C.isAlpha phrase
       in onlyLetters /= "" && T.all C.isUpper onlyLetters

    isLoudQuestion phrase = isQuestion phrase && isLoud phrase


-- Tests
testEmpty :: Test
testEmpty = TestCase (
  assertEqual
    "Testing empty input"
    "Fine. Be that way!"
    (responseFor "   \r\n   \t"))

testYelling :: Test
testYelling = TestCase (
  assertEqual
    "Testing yelling input"
    "Whoa, chill out!"
    (responseFor "HELLO!!!"))

testYelledQuestion :: Test
testYelledQuestion = TestCase (
  assertEqual
    "Testing yelled question"
    "Calm down, I know what I'm doing!"
    (responseFor "HOW ARE YOU?"))

testQuestion :: Test
testQuestion = TestCase (
  assertEqual
    "Testing question"
    "Sure."
    (responseFor "How are you?"))

testOther :: Test
testOther = TestCase (
  assertEqual
    "Testing normal input"
    "Whatever."
    (responseFor "Hello, World!"))

testNumbers :: Test
testNumbers = TestCase (
  assertEqual
    "Testing only numbers"
    "Whatever."
    (responseFor "1234"))

testNumberQuestion :: Test
testNumberQuestion = TestCase (
  assertEqual
    "Test question with only numbers"
    "Sure."
    (responseFor "1234?"))

testOtherQuestion :: Test
testOtherQuestion = TestCase (
  assertEqual
    "Test question with non-letters"
    "Sure."
    (responseFor "*&^%?"))

tests :: Test
tests = TestList [
  TestLabel "Empty string" testEmpty,
  TestLabel "Yelling" testYelling,
  TestLabel "Yelling questions" testYelledQuestion,
  TestLabel "Questioning" testQuestion,
  TestLabel "Other input" testOther,
  TestLabel "Other input" testNumbers,
  TestLabel "Other input" testNumberQuestion,
  TestLabel "Other input" testOtherQuestion
  ]

runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()
