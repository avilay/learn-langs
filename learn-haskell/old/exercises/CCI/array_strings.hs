{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set as S

test :: (Show a, Show b, Eq b) => (a -> b) -> [a] -> [b] -> String
test f cases exp
  | exp == act = "Pass"
  | otherwise = "FAIL - " ++ show failed
  where
    act = map f cases
    failed = filter (\(_, expected, actual) -> expected /= actual) $ zip3 cases exp act

isUnique :: String -> Bool
isUnique cs = length (S.fromList cs) == length cs

testIsUnique :: String
testIsUnique =
  let tests = ["", "aa", "abc", "b", "aabbcc", "aabc"]
      exp = [True, False, True, True, False, False]
   in "isUnique: " ++ test isUnique tests exp

arePermutations :: String -> String -> Bool
arePermutations string1 string2 = L.sort string1 == L.sort string2

testArePermutations :: String
testArePermutations =
  let tests = [("", ""), ("god", "dog"), ("hello", "world")]
      exp = [True, True, False]
   in "arePermutations: " ++ test (uncurry arePermutations) tests exp

urlify :: String -> String
urlify string = foldr _urlify "" (_trim string)
  where
    _trim = f . f
      where
        f = reverse . dropWhile C.isSpace
    _urlify c str
      | c == ' ' = "%20" ++ str
      | otherwise = c : str

testUrlify :: String
testUrlify =
  let tests = ["hello world", "hello", "    hello    ", "", "  hello  world  "]
      exp = ["hello%20world", "hello", "hello", "", "hello%20%20world"]
   in "urlify: " ++ test urlify tests exp

isPalindromePermutation :: String -> Bool
isPalindromePermutation str
  | even $ length canonStr = (length . filter (odd . snd) . counter) canonStr == 0
  | otherwise = (length . filter (odd . snd) . counter) canonStr == 1
  where
    counter = map (\xs -> (head xs, length xs)) . L.group . L.sort
    zapSpaces = foldr (\c str -> if c == ' ' then str else c : str) ""
    canonStr = map C.toLower $ zapSpaces str

testIsPalindromePermutation :: String
testIsPalindromePermutation =
  let tests = ["Tact Coa", "Hello World"]
      exp = [True, False]
   in "isPalindromePermutation: " ++ test isPalindromePermutation tests exp

compress :: String -> String
compress str
  | length str > length compressed = compressed
  | otherwise = str
  where
    compressed = foldr (\(c, n) cs -> c : (show n ++ cs)) "" (countChars str)
    countChars = map (\xs -> (head xs, length xs)) . L.group

testCompress :: String
testCompress =
  let tests = ["aabcccccaaa", "aaaa", "abcd", "aabb"]
      exp = ["a2b1c5a3", "a4", "abcd", "aabb"]
   in "compress: " ++ test compress tests exp

oneAway :: String -> String -> Bool
oneAway s [] = length s == 1
oneAway [] s = length s == 1
oneAway (c1 : cs1) (c2 : cs2)
  | abs (length cs1 - length cs2) > 1 = False
  | c1 == c2 = oneAway cs1 cs2
  | length cs1 > length cs2 = cs1 == c2 : cs2
  | length cs1 < length cs2 = c1 : cs1 == cs2
  | otherwise = cs1 == cs2 -- when length cs1 == length cs2

testOneAway :: String
testOneAway =
  let tests = [("pale", "ple"), ("pales", "pale"), ("pale", "bale"), ("pale", "bae"), ("apple", "aple"), ("hello", "worl")]
      exp = [True, True, True, False, True, False]
   in "testOneAway: " ++ test (uncurry oneAway) tests exp

main = do
  let msgs =
        [ "**** TEST RESULTS ****",
          testIsUnique,
          testArePermutations,
          testUrlify,
          testIsPalindromePermutation,
          testCompress,
          testOneAway
        ]
  putStrLn $ L.unlines msgs

data Matrix = EmptyMatrix | Matrix {rows :: [[Int]]}

row :: Int -> Matrix -> [Int]
row r EmptyMatrix = error "wtf?!"
row r (Matrix rows) = rows !! r

col :: Int -> Matrix -> [Int]
col c EmptyMatrix = error "wtf?!"
col c (Matrix rows) = [row !! c | row <- rows]

nRows :: Matrix -> Int
nRows EmptyMatrix = 0
nRows (Matrix rows) = length rows

nCols :: Matrix -> Int
nCols EmptyMatrix = 0
nCols (Matrix rows) = length . head $ rows

instance Show Matrix where
  show EmptyMatrix = "[]"
  show (Matrix rows) = drop 1 $ foldl (\acc row -> acc ++ "\n" ++ show row) "" rows

rotate :: Matrix -> Matrix
rotate EmptyMatrix = EmptyMatrix
rotate mat = Matrix [col i mat | i <- [0 .. nCols mat -1]]