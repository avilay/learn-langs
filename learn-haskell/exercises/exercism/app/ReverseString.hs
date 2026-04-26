module ReverseString where

import Test.HUnit

append :: Char -> String -> String
append char acc = acc ++ [char]

reverse :: String -> String
reverse = foldr append ""

-- Test cases

testEmptyString :: Test
testEmptyString = TestCase (assertEqual "empty string" "" (ReverseString.reverse ""))

testSingleCharacter :: Test
testSingleCharacter = TestCase (assertEqual "single character" "a" (ReverseString.reverse "a"))

testFullString :: Test
testFullString = TestCase (assertEqual "full string" "hello world" (ReverseString.reverse "dlrow olleh"))

-- Test suite
tests :: Test
tests = TestList
    [ 
      TestLabel "empty string" testEmptyString, 
      TestLabel "single character" testSingleCharacter,
      TestLabel "full string" testFullString
    ]

-- Main function to run all tests
runTests :: IO ()
runTests = do
    result <- runTestTT tests
    if failures result > 0 || errors result > 0
        then putStrLn "\nSome tests failed!"
        else putStrLn "\nAll tests passed!"
