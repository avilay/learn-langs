-- |
-- Module                  : Lecture1
-- Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
-- SPDX-License-Identifier : MPL-2.0
-- Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
-- Stability               : Stable
-- Portability             : Portable
--
-- Exercises for the Lecture 1 of the Haskell Beginners course.
--
-- To complete exercises, you need to complete implementation and add
-- missing top-level type signatures. You can implement any additional
-- helper functions. But you can't change the names of the given
-- functions.
--
-- Comments before each function contain explanations and example of
-- arguments and expected returned values.
--
-- It's absolutely okay if you feel that your implementations are not
-- perfect. You can return to these exercises after future lectures and
-- improve your solutions if you see any possible improvements.
module Lecture1
  ( makeSnippet,
    sumOfSquares,
    lastDigit,
    minmax,
    subString,
    strSum,
    lowerAndGreater,
  )
where

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^ ^^ and before this line. Otherwise the test suite might fail  ^^^

-- | Specify the type signature of the following function. Think about
-- its behaviour, possible types for the function arguments and write the
-- type signature explicitly.
makeSnippet :: Int -> [Char] -> [Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

-- | Implement a function that takes two numbers and finds sum of
-- their squares.
--
-- >>> sumOfSquares 3 4
-- 25
--
-- >>> sumOfSquares (-2) 7
-- 53
--
-- Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
-- is 25.

-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = x ^ 2 + y ^ 2

-- | Implement a function that returns the last digit of a given number.
--
-- >>> lastDigit 42
-- 2
-- >>> lastDigit (-17)
-- 7
--
-- 🕯 HINT: use the @mod@ functionn

-- DON'T FORGET TO SPECIFY THE TYPE IN HERE
lastDigit :: Int -> Int
lastDigit n = abs n `mod` 10

-- | Write a function that takes three numbers and returns the
-- difference between the biggest number and the smallest one.
--
-- >>> minmax 7 1 4
-- 6
--
-- Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
-- and 1 is the smallest, and 7 - 1 = 6.
--
-- Try to use local variables (either let-in or where) to implement this
-- function.
minmax :: Int -> Int -> Int -> Int
minmax x y z =
  let big = max (max x y) z
      small = min (min x y) z
   in big - small

-- | Implement a function that takes a string, start and end positions
-- and returns a substring of a given string from the start position to
-- the end (including).
--
-- >>> subString 3 7 "Hello, world!"
-- "lo, w"
--
-- >>> subString 10 5 "Some very long String"
-- ""
--
-- This function can accept negative start and end position. Negative
-- start position can be considered as zero (e.g. substring from the
-- first character) and negative end position should result in an empty
-- string.
subString :: Int -> Int -> String -> String
subString start end str
  | end < 0 = ""
  | start < 0 = take (end + 1) str
  | otherwise = take (end - start + 1) (drop start str)

-- | Write a function that takes a String — space separated numbers,
-- and finds a sum of the numbers inside this string.
--
-- >>> strSum "100    -42  15"
-- 73
--
-- The string contains only spaces and/or numbers.
-- Official solution is
-- strSum str = sum (map read (words str))
-- `words` is like the split function I implemented.
-- `read` parses the string into whatever type is specified, e.g., `read "10" :: Int`
-- but when the type is not specified, it will try to infer the type. `sum` function
-- provides the inference that the output of read is expected to be a Num.
toDigit :: Char -> Int
toDigit '0' = 0
toDigit '1' = 1
toDigit '2' = 2
toDigit '3' = 3
toDigit '4' = 4
toDigit '5' = 5
toDigit '6' = 6
toDigit '7' = 7
toDigit '8' = 8
toDigit '9' = 9

strToInt :: String -> Int
strToInt str =
  if head str == '-'
    then (-1) * go 0 0 (tail str)
    else go 0 0 str
  where
    go _ n "" = n
    go p n txt =
      let p' = p + 1
          d = toDigit (last txt)
          n' = (10 ^ p) * d + n
          txt' = init txt
       in go p' n' txt'

split :: [String] -> String -> [String]
split words "" = filter (/= "") words
split words text =
  if head text == ' '
    then split (words ++ [""]) (tail text)
    else
      let firstChar = head text
          lastWord = last words ++ [firstChar]
          newWords = init words ++ [lastWord]
          newText = tail text
       in split newWords newText

strSum :: String -> Int
strSum str =
  let words = split [""] str
      nums = map strToInt words
   in sum nums

-- | Write a function that takes a number and a list of numbers and
-- returns a string, saying how many elements of the list are strictly
-- greater than the given number and strictly lower.
--
-- >>> lowerAndGreater 3 [1 .. 9]
-- "3 is greater than 2 elements and lower than 6 elements"
--
-- Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- The given number 3 is greater than 2 elements (1 and 2)
-- and lower than 6 elements (4, 5, 6, 7, 8 and 9).
--
-- 🕯 HINT: Use recursion to implement this function.
lowerAndGreater :: Int -> [Int] -> String
lowerAndGreater = go 0 0
  where
    go :: Int -> Int -> Int -> [Int] -> String
    go numLess numGreater pivot [] = show pivot ++ " is greater than " ++ show numLess ++ " elements and lower than " ++ show numGreater
    go numLess numGreater pivot ary
      | head ary > pivot = go numLess (numGreater + 1) pivot (tail ary)
      | head ary == pivot = go numLess numGreater pivot (tail ary)
      | otherwise = go (numLess + 1) numGreater pivot (tail ary)
