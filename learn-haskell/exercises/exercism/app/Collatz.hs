module Collatz where

import Test.HUnit

collatz :: Integer -> Maybe Integer
collatz n = if n <= 0 then Nothing else Just (go 0 n)
  where
    go :: Integer -> Integer -> Integer
    go steps 1 = steps
    go steps m
      | even m = go (steps + 1) (m `div` 2)
      | otherwise = go (steps + 1) (3*m + 1)

-- Tests
testEven :: Test
testEven = TestCase (assertEqual "Testing 12" (Just 9) (collatz 12))

testOdd :: Test
testOdd = TestCase (assertEqual "Testing 13" (Just 9) (collatz 13))

testZero :: Test
testZero = TestCase (assertEqual "Testing 0" Nothing (collatz 0))

testNegative :: Test
testNegative = TestCase (assertEqual "Testing -3" Nothing (collatz (-13)))

tests :: Test
tests = TestList [
  TestLabel "one" testEven,
  TestLabel "two" testOdd,
  TestLabel "three" testZero,
  TestLabel "four" testNegative
  ]

runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()
