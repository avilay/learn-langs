module Main where

import qualified ReverseString
import qualified Darts
import qualified RotationalCipher
import qualified LeapYear
import qualified SpaceAge
import qualified Pangram
import qualified Bob
import qualified Collatz
import qualified DNA

main :: IO ()
main = do
  putStrLn "Running ReverseString tests..."
  ReverseString.runTests
  
  putStrLn "\nRunning Darts tests..."
  Darts.runTests

  putStrLn "\nRunning RotationalCipher tests..."
  RotationalCipher.runTests

  putStrLn "\nRunning LeapYear tests..."
  LeapYear.runTests

  putStrLn "\nRunning SpaceAge tests..."
  SpaceAge.runTests

  putStrLn "\nRunning Pangram tests..."
  Pangram.runTests

  putStrLn "\nRunning Bob tests..."
  Bob.runTests

  putStrLn "\nRunning Collatz tests..."
  Collatz.runTests

  putStrLn "\nRunning DNA tests..."
  DNA.runTests

