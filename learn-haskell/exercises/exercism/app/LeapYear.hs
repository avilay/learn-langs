module LeapYear where

import Test.HUnit

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy divisor dividend = dividend `mod` divisor == 0

-- isLeapYear :: Int -> Bool
-- isLeapYear year = (isDivisibleByFour year && (not . isDivisibleByHundred $ year)) || isDivisibleByFourHundred year
--   where
--     isDivisibleByFour = isDivisibleBy 4
--     isDivisibleByHundred = isDivisibleBy 100
--     isDivisibleByFourHundred = isDivisibleBy 400

-- isLeapYear :: Int -> Bool
-- isLeapYear year
--   | isDivisibleBy 400 year = True
--   | (not . isDivisibleBy 100 $ year) && isDivisibleBy 4 year = True
--   | otherwise = False

isLeapYear :: Integer -> Bool
isLeapYear year = divBy 4 && (not . divBy $ 100) || divBy 400
  where
    divBy n = year `mod` n == 0

-- Test cases

testNormalLeapYear :: Test
testNormalLeapYear = TestCase 
  (assertBool "Testing normal leap year" (isLeapYear 2024))

testTrickyLeapYear :: Test
testTrickyLeapYear = TestCase
  (assertBool "Testing tricky leap year" (isLeapYear 2000))

testNotLeapYear :: Test
testNotLeapYear = TestCase
  (assertBool "Testing a non leap year" (not . isLeapYear $ 1997))

-- Test suite
tests :: Test
tests = TestList [
  TestLabel "leap year" testNormalLeapYear,
  TestLabel "leap year" testTrickyLeapYear,
  TestLabel "non leap year" testNotLeapYear
  ]

runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()
