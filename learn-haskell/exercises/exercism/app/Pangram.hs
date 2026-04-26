module Pangram where

import Data.Char
-- import qualified Data.Set as Set
import Test.HUnit

isPangram :: String -> Bool
-- isPangram text =
--   let letters = Set.fromList . map toLower . filter isAlpha $ text
--       alphabets = Set.fromList ['a'..'z']
--    in alphabets `Set.isSubsetOf` letters

isPangram text = and [alphabet `elem` letters | alphabet <- ['a'..'z']]
  where
    letters = map toLower . filter isAlpha $ text


-- Tests
testValid :: Test
testValid = TestCase (assertBool "Testing valid pangram" (isPangram "The quick brown fox jumps over the lazy dog."))

testInvalid :: Test
testInvalid = TestCase (assertBool "Testing invalid pangram" (not. isPangram $ "Hello, World!"))

testNonLetters :: Test
testNonLetters = TestCase (assertBool "Testing non letters" (not. isPangram $ "*&:/"))

testEmpty :: Test
testEmpty = TestCase (assertBool "Testing empty" (not . isPangram $ ""))

-- Test suite
tests :: Test
tests = TestList [
  TestLabel "is a pangram" testValid,
  TestLabel "not a pangram" testInvalid,
  TestLabel "not a pangram" testNonLetters,
  TestLabel "not a pangram" testEmpty
  ]

runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()
