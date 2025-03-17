{-
A triple (x, y, z) of positive integers is called a pythagorean if x^2 + y^2 = z^2. Using list comprehension define a
function to find all pythagorean triples less than a bound.

pyths 5 = [(3, 4, 5), (4, 3, 5)]
-}
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

{-
A positive integer is perfect is it equals the sum of all its factors, excluding the number itself. Define a function
that will list all the perfects less than a bound.
    perfects 500 = [6, 28, 496]
6 is a perfect number because its factors are 1, 2, 3 and their sum is 6.
28 is a perfect number because its factors are 1, 2, 4, 7, 14 and their sum is 28.
And so on.

Simple implementation
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], perfect x]

perfect :: Int -> Bool
perfect n = sum (factors n) == n

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n - 1], n `mod` x == 0]

Below is an implementation without littering the global namespace with too many functions.
-}
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], perfect x]
  where
    perfect m = sum (init (factors m)) == m
    factors r = [y | y <- [1 .. r], r `mod` y == 0]

{-
Dot product of two vectors, represented as lists.
-}
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum [x * y | (x, y) <- zip xs ys]
