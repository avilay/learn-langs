# Recursive Functions

Given I already know what recursion is, this note will just have cool examples of recursion.

## Fun Examples

### Good old factorial

```haskell
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)
```

The above definition is incomplete because it does not define `fac -1` which will throw a stack overflow exception.

### Product of a list of numbers

```haskell
prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs
```

### Length of a list

```haskell
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len(xs)
```

### Reverse a list

```haskell
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]
```

### Zip

```haskell
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys 
```

### Drop

```haskell
myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (x:xs) = myDrop (n-1) xs
```

### Concat Lists

```haskell
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys
```

### Quick sort

```haskell
qsort :: Ord a => [a] -> [a]
qsort (x:xs) = smaller ++ [x] ++ bigger
  where
    smaller = [x' | x' <- xs, x' <= x]
    larger = [x' | x' <- xs, x' > x]
```

Equivalent definition in Python

```python
def qsort(ary):
    if not ary: return []
    pivot = ary[0]
    smaller = [x for x in ary[1:] if x < pivot]
    larger = [x for x in ary[1:] if x >= pivot]
    return qsort(smaller) + [pivot] + qsort(larger)
```

