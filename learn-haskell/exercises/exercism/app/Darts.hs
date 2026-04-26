module Darts where
import Test.HUnit

data Point = Point { x :: Float, y :: Float}

distanceFromOrigin :: Point -> Float
distanceFromOrigin p = sqrt ((x p^(2::Integer)) + (y p^(2::Integer)))

score :: Point -> Integer
score dart
  | r <= 1 = 10
  | r <= 5 = 5
  | r <= 10 = 1
  | otherwise = 0
  where r = distanceFromOrigin dart


-- Test cases

runTest :: (Point, String, Integer) -> Assertion
runTest (point, msg, expected) = assertEqual msg expected (score point)

testHappy :: Test
testHappy = TestCase $ mapM_ runTest testData
  where
    testData = [
      (Point 0.1 0.2, "center", 10),
      (Point 3.5 3.5, "close", 5),
      (Point 7.4 6.1, "very close", 1),
      (Point 5.2 8.9, "nope", 0)
      ]

testEdge :: Test
testEdge = TestCase $ mapM_ runTest testData
  where
    testData = [
      (Point 0 0, "dead center", 10),
      (Point 3 4, "on the edge", 5),
      (Point 7 7, "on the very edge", 1),
      (Point 12 13, "nope", 0)
      ]
    

-- Test suite
tests :: Test
tests = TestList [
  TestLabel "Happy case" testHappy,
  TestLabel "Edge case" testEdge
  ]

runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()