# List Comprehensions

A good way to think about this is to think of how we define sets in math $A = \{x^2 : x \in \{1, 2, 3\} \}$ we define list comprehensions using similar syntax -

```haskell
[x^2 | x <- [1..5]]
[1, 4, 9, 16, 25]
```

Comprehensions can have nested generators (the left arrow operator) with multiple nesting levels and each level can reference the variables in the outer generators. Another way to think about nested generators is that the inner ones change their values more rapidly than the outer ones.

```haskell
-- y's values change faster than x's values
> [(x, y) | x <- [1, 2, 3], y <- [4, 5]]
[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- the order in which they are placed in the tuple is irrelevant
> [(y, x) | x <- [1, 2, 3], y <- [4, 5]]
[(4,1),(5,1),(4,2),(5,2),(4,3),(5,3)]

-- dependent generator
> [(x, y) | x <- [1, 2, 3], y <- [x..3]]
[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
```

 A cool way to flatten multiple nested lists into a single list using nested generators -

```haskell
flatten :: [[a]] -> [a]
flatten xss = [x | xs <- xss, x <- xs]
```

Just like in Python, list comprehensions can be conditional -

```haskell
[x | x <- [1..10], even x]
```

## Fun Examples

#### Find all primes less than a bound

```haskell
-- Find all the factors of an integer
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- Check whether a given number is prime or not
prime :: Int -> Bool
prime n = factors n == [1, n]

-- Find all prime numbers below a bound
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
```

#### Check if a list is sorted

```haskell
-- Return pairs of adjacent elements in a list
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- Check if a list is sorted or not
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]
```

#### Get indexes of an element from a list

```haskell
indexes :: Eq a => a -> [a] -> [Int]
indexes x xs = [i | (x', i) <- zip xs [0..], x == x']
```

#### Count the number of times a character is repeated in a string

```haskell
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
```

  

