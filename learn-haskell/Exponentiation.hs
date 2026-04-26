module Exponentiation where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Debug.Trace

powerV1 :: Integer -> Integer -> Integer
powerV1 _ 0 = 1
powerV1 b n = b * powerV1 b (n-1)

powerV2 :: Integer -> Integer -> Integer
powerV2 b = power_iter 1
  where
    power_iter :: Integer -> Integer -> Integer
    power_iter acc 0 = acc
    power_iter acc ctr = power_iter (acc * b) (ctr - 1)


powerV3 :: Integer -> Integer -> Integer
powerV3 _ 0 = 1
powerV3 b n
  | even n = powerV3 (b * b) (n `div` 2)
  | otherwise = b * powerV3 b (n - 1)


powerV4 :: Integer -> Integer -> Integer
powerV4 = _power_iter 1
  where
    _power_iter :: Integer -> Integer -> Integer -> Integer
    _power_iter acc _ 0 = acc
    _power_iter acc b expt
        | even expt = _power_iter acc (b * b) (expt `div` 2)
        | otherwise = _power_iter (acc * b) b (expt - 1)


main :: IO ()
main = do
  args <- getArgs
  if length args /= 3
    then do
      putStrLn "Error: Expected exactly 3 arguments"
      putStrLn "Usage: program <v1|v2|v3|v4> b n"
      exitFailure
    else do
      let version = head args
      let b = read (args !! 1) :: Integer
      let n = read (args !! 2) :: Integer
      let result = case version of
                    "v1" -> powerV1 b n
                    "v2" -> powerV2 b n
                    "v3" -> powerV3 b n
                    "v4" -> powerV4 b n
                    _   -> error "First argument must be v1, v2, v3, or v4"
      print result
