module DNA where

import Test.HUnit


-- Lookup table for DNA to RNA nucleotide conversion
nucleotideToRNA :: Char -> Either Char Char
nucleotideToRNA 'G' = Right 'C'
nucleotideToRNA 'C' = Right 'G'
nucleotideToRNA 'T' = Right 'A'
nucleotideToRNA 'A' = Right 'U'
nucleotideToRNA c = Left c

-- Idiomatic Haskell: use traverse to map over the string
-- traverse applies nucleotideToRNA to each character and sequences the Either effects
toRNA :: String -> Either Char String
toRNA = traverse nucleotideToRNA


-- Test cases
testEmpty :: Test
testEmpty = TestCase (assertEqual "Testing empty" (Right "") (toRNA ""))

runTest :: (String, String, Either Char String) -> Assertion
runTest (dna, msg, expected) = assertEqual msg expected (toRNA dna)

testSingle :: Test
testSingle = TestCase $ mapM_ runTest testData
  where
    testData = [
      ("G", "Testing G", Right "C"),
      ("C", "Testing C", Right "G"),
      ("T", "Testing T", Right "A"),
      ("A", "Testing A", Right "U")
      ]

testRnaComplement :: Test
testRnaComplement = TestCase (assertEqual "Testing RNA complement" (Right "UGCACCAGAAUU") (toRNA "ACGTGGTCTTAA"))

testRnaToRna :: Test
testRnaToRna = TestCase (assertEqual "Testing RNA to RNA" (Left 'U') (toRNA "U"))

testFullyBad :: Test
testFullyBad = TestCase (assertEqual "Testing fully bad sequence" (Left 'X') (toRNA "XXX"))

testPartlyBad :: Test
testPartlyBad = TestCase (assertEqual "Testing partly bad sequence" (Left 'X') (toRNA "ACGTXXXCTTAA"))

-- Test suite
tests :: Test
tests = TestList [
  TestLabel "Empty" testEmpty,
  TestLabel "Single" testSingle,
  TestLabel "RNA Complement" testRnaComplement,
  TestLabel "RNA to RNA" testRnaToRna,
  TestLabel "Fully bad" testFullyBad,
  TestLabel "Partly bad" testPartlyBad
  ]

runTests :: IO ()
runTests = do
  _ <- runTestTT tests
  return ()
