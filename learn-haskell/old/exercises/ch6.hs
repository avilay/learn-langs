-- Decide if all logical values in a list are true
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

-- Flatten a list of lists
flatten :: [[a]] -> [a]
flatten [] = []
flatten (xs : xss) = xs ++ flatten xss

-- Produce a list with n identical elements
fill :: Int -> a -> [a]
fill 0 _ = []
fill n x = x : fill (n - 1) x

-- Select the nth element of a list
elemAt :: [a] -> Int -> a
elemAt [] _ = error "Cannot get element from empty list!"
elemAt (x : xs) n
  | n < 0 = error "Negative index not allowed!"
  | n == 0 = x
  | otherwise = elemAt xs (n -1)

-- Check if value is in list
contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains y (x : xs)
  | x == y = True
  | otherwise = contains y xs

{-
Define a recursive function that merges two sorted lists to give a single sorted list.
    merge [2, 5, 6] [1, 3, 4] = [1, 2, 3, 4, 5, 6]
-}
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

{-
Define a recursive function that implements merge sort which can be specified by two rules:
  - Lists of length <= 1 are already sorted
  - Other lists can be sorted by sorting the two halves and merging the resulting lists.
-}
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where
    left = take n xs
    right = drop n xs
    n = length xs `div` 2
