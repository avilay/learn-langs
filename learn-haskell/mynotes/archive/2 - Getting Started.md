# First Steps

Simple Haskell function

```Haskell
double x = x + x
quadruple x = double (double x)
```

Here is a good table showing how Haskell definitions work -

<img src="./haskell_1.png" alt="haskell_1" style="zoom:25%;" />

## Basic Syntax

### Partial by default

All functions are partial by default.

```haskell
> myAdd x y = x + y
> tp = myAdd 1
> tp 2
3
```

### Boolean values

Just like in Python, there are two boolean values - `True` and `False`.

### Binary functions are special

The usual way to call any function is `f x y`. But if a funciton is binary, i.e., it accepts two values, then it can be called as -

```haskell
x `f` y
```

### (Un)common operators

#### Division

By default all division is floating point when using the `/` operator. To do integral division use the `div` function.

```haskell
-- normal (fractional) division
10/3
3.333333333335

-- integral division
10 `div` 3
3
```

#### Not-Equals

The not-equals operator is written as `/=`, e.g., `1 /= 2` will evaluate to `True`.

### Wildcards

Just like in Python `_` is a wildcard that means that the program does not really care about the value in this argument. E.g., this function `f _ 0 = 0` will accept two arguments but if the second argument is 0, it will always return a 0 regardless of the first argument.

### Throwing errors

Use the `error` function to throw an error. `bad _ = error “cannot allow that!”` is a function that will always throw an error no matter the input.

### As-Pattern

Syntactic sugar to break a composite data type into its components. `ps@(p:pt)` gives me access to all three values - the original list `ps`, the head of the list `p` and the tail of the list `pt` at once.

I can also use some combination like 

### List functions

Lists are one of the most common data types in Haskell. There are bunch of functions defined in the `Prelude` which is its standard library to work on lists.

```haskell
-- Range operator
> [1..3]
[1,2,3]

> [1..]
--will give an infinite list

-- Checking for empty list
> null [1, 2, 3]
False

> null []
True

-- Prepending to a list using the cons (:) operator
> 1 : [2, 3]
[1,2,3]

-- Concatenating two lists
> [1, 2, 3] ++ [4, 5]
[1,2,3,4,5]

-- length of a list
> length [1, 2, 3]
3

> length []
0

-- element at index
> ['A', 'B', 'C'] !! 1
'B'

-- first element of a list
> head [1, 2, 3]
1

-- all but first elements of a list
> tail [1, 2, 3]
[2, 3]

-- all but the last element of a list
> init [1, 2, 3]
[1, 2]

-- last element of a list
> last [1, 2, 3]
3

-- first n elements
take 3 [1, 2, 3, 4, 5]
[1, 2, 3]

-- all but first n elements
> drop 3 [1, 2, 3, 4, 5]
[4, 5]

-- sum of all elements in a list
> sum [1, 2, 3]
6

-- product of all elements in a list
> product [1, 2, 3]
6

-- reverse a list
> reverse [1, 2, 3]
[3, 2, 1]


-- map a given function to all elements of a list
> f x = x ^ 2
> map f [1, 2, 3]
[1, 4, 9]

-- zip takes two lists and creates a single list
> zip [1, 2, 3] ['a', 'b']
[(1,'a'),(2,'b')]

-- maximum/minimum element
> maximum [4, 3, 8, 1]
8

> minimum [4, 3, 8, 1]
1

-- checks if element is in the list
> 2 `elem` [4, 3, 8, 1]
False

-- fills a list with the same value
> replicate 3 10
[10,10,10]
```

### Multiple Lines

```haskell
usingLet x =
  let y = 10
      z = foo y
   in x + y - z
   
usingWhere x = go 10 x
  where
    go a b = a + b
```

### If-Then-Else

```haskell
usingIf x = 
  if x > 10 
    then "Big" 
    else "Small"

usingGuards x
  | x < 0 = "Negative"
  | x == 0 = "Zero"
  | otherwise = "Positive"
```

### Pattern Matching

In Haskell a common way to implement functions is to use pattern matching. Best explained by examples -

```haskell
-- Literals in function signature
eval :: Char -> Int -> Int -> Int
eval '+' x y = x + y
eval '-' x y = x - y
eval '*' x y = x * y
eval '/' x y = x `div` y
eval _ _ _ = 0

-- Using case-of
calc :: Char -> Int -> Int -> Int
calc op x y = case op of
  '+' -> x + y
  '-' -> x - y
  '*' -> x * y
  '/' -> x `div` y
  _ -> 0
```

