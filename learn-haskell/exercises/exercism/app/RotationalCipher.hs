module RotationalCipher where

import Data.Char (isUpper, isAlpha, ord, chr)
import Test.HUnit

rotateChar :: Int -> Char -> Char
rotateChar offset char
  | not (isAlpha char) = char
  | otherwise =
      let base = if isUpper char then ord('A') else ord('a')
          pos = ord(char) - base
          newPos = (pos + offset) `mod` 26
      in chr(newPos + base)

rotate :: Int -> String -> String
rotate offset = map (rotateChar offset)


-- Test cases

testHappy :: Test
testHappy = TestCase $ mapM_ runHappyTest testData
  where
    testData = [
      (5, "omg", "trl"),
      (0, "c", "c"),
      (26, "Cool", "Cool"),
      (13, "The quick brown fox jumps over the lazy dog.", "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."),
      (13, "Gur dhvpx oebja sbk whzcf bire gur ynml qbt.", "The quick brown fox jumps over the lazy dog.")
      ]
    runHappyTest (offset, plainText, cipherText) = assertEqual "Happy test fail - " cipherText (rotate offset plainText)

-- Test suite
tests :: Test
tests = TestList [
  TestLabel "Happy case" testHappy
  ]    

runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()
