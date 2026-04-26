module ReverseStringTest where

import Test.HUnit
import qualified Exercism

-- Test cases for Exercism.reverse function

testEmptyString :: Test
testEmptyString = TestCase (assertEqual "empty string" "" (Exercism.reverse ""))

testSingleCharacter :: Test
testSingleCharacter = TestCase (assertEqual "single character" "a" (Exercism.reverse "a"))

testSimpleWord :: Test
testSimpleWord = TestCase (assertEqual "simple word" "tobor" (Exercism.reverse "robot"))

testPalindrome :: Test
testPalindrome = TestCase (assertEqual "palindrome" "racecar" (Exercism.reverse "racecar"))

testWithSpaces :: Test
testWithSpaces = TestCase (assertEqual "string with spaces" "dlrow olleh" (Exercism.reverse "hello world"))

testWithNumbers :: Test
testWithNumbers = TestCase (assertEqual "string with numbers" "54321" (Exercism.reverse "12345"))

testWithSpecialChars :: Test
testWithSpecialChars = TestCase (assertEqual "string with special characters" "!dlrow ,olleH" (Exercism.reverse "Hello, world!"))

testMultipleWords :: Test
testMultipleWords = TestCase (assertEqual "multiple words" "tset a si sihT" (Exercism.reverse "This is a test"))

testWithPunctuation :: Test
testWithPunctuation = TestCase (assertEqual "string with punctuation" "?siht si tahW" (Exercism.reverse "What is this?"))

testLongString :: Test
testLongString = TestCase (assertEqual
    "long string"
    "gnirts gnol yrev yrev a si sihT"
    (Exercism.reverse "This is a very very long string"))

testWithTabs :: Test
testWithTabs = TestCase (assertEqual "string with tabs" "\tB\tA" (Exercism.reverse "A\tB\t"))

testWithNewlines :: Test
testWithNewlines = TestCase (assertEqual "string with newlines" "\nenilwen\n" (Exercism.reverse "\nnewline\n"))

testUnicodeCharacters :: Test
testUnicodeCharacters = TestCase (assertEqual "unicode characters" "👋🌍" (Exercism.reverse "🌍👋"))

-- Test suite
tests :: Test
tests = TestList
    [ TestLabel "empty string" testEmptyString
    , TestLabel "single character" testSingleCharacter
    , TestLabel "simple word" testSimpleWord
    , TestLabel "palindrome" testPalindrome
    , TestLabel "with spaces" testWithSpaces
    , TestLabel "with numbers" testWithNumbers
    , TestLabel "with special chars" testWithSpecialChars
    , TestLabel "multiple words" testMultipleWords
    , TestLabel "with punctuation" testWithPunctuation
    , TestLabel "long string" testLongString
    , TestLabel "with tabs" testWithTabs
    , TestLabel "with newlines" testWithNewlines
    , TestLabel "unicode characters" testUnicodeCharacters
    ]

-- Main function to run all tests
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 || errors result > 0
        then putStrLn "\nSome tests failed!"
        else putStrLn "\nAll tests passed!"
