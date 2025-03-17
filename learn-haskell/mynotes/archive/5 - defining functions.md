# Defining Functions

# Syntax

### `where`

This can be used in defining expressions as follows -

```haskell
a = b + c
    where
      b = 1
      c = 2
```

`where` can be used to define any values, including functions -

```haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
  where what [] = "empty."  
        what [x] = "a singleton list."  
        what xs = "a longer list."  
```

## `let…in`

Just like `where`, `let…in` also defines values, but these are actually expressions unlike `where` which is just syntactic sugar.

```haskell
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
```

The entire `let…in` construct is an expression that will evaluate to the value of `sideArea` which is then returned as the answer of the `cylinder` function. `let…in` can also be used list comprehensions and such. The drawback of this construct is that its scope is limited, it does not span across guards (see later).

### Operator Overloading

While other functions are defined as `foo a b = a + b`, overloaded operators are defined as they would be used, `a ^ 2 = a * a`. 

### `if-then-else`

There are a number of ways to do this, best explained by examples.

```haskell
-- simple if-then-else
abs :: Int -> Int
abs n = if n >= 0 then n else -n

-- nested ifs
signum :: Int -> Int
signum n = if n < 0 then -1 else
             if n == 0 then 0 else 1
```

All `if` statements **must** have corresponding `else`. Another way to write multiple nested ifs is to use the `|` operator called **guarded ifs**.

```haskell
abs n | n >= 0 = 0
      | otherwise = -n
      
signum n | n < 0 = -1
         | n == 0 = 0
         | otherwise = 1
```

### wildcard

Similar to Python, `_` symbol is a wild card argument, indicating that we don't really care about its value. E.g., `safeDiv _ 0 = 0`. 

### List as an input argument

I can use the cons operator `:` in function arguments and it has a similar but slightly different meaning.

```haskell
head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs
```

I have to use parens when using cons, e.g., `head x:_ = x` is wrong because Haskell will first try to apply the cons operator to `x` and the wildcard `_` and get confused. Also the cons operator implies that the input is a non-empty list. Calling `head []` will result in an exception.

## Pattern Matching

This is a new way of defining functions where the arguments are "hardcoded" and the function is evaluated in the order it is written. Again, best explained by an example -

```haskell
not :: Bool -> Bool
not False = True
not True = False
```

This means that when evaluating the function `not`, the compiler will first check against the first pattern defined and check if the argument passed in is `False`, if so it will evaluate to `True` and the function evaluation is complete. If the first argument is not `False`, then the compiler will look at the second pattern and see that it is defined for when the argument is `True` and so on. Here are multiple ways of defining the `&&` operator -

```haskell
(&&) :: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False

-- alternate 1
True && True = True
_ && _ = False

-- alternate 2
True && b = b
False && _ = False
```

Another example with a bunch of above concepts combined -

```haskell
(**) :: (Num a2, Integral a1) => a2 -> a1 -> a2
x ** 0 = 1
x ** 1 = x
x ** n
  | even n = h * h
  | otherwise = x * h * h
  where
    h = x Main.** (n `div` 2)
```

## Lambda

The usual anonymous functions. Most useful for defining single use function. The syntax is as follows -

```haskell
-- lambda with 2 input args
f = \x y -> x + y

-- lambda being used directly
map (\x -> x^2) [1, 2, 3]
[1, 4, 9]
```

## Operator Sections

All binary operators can be called in post-fix form, i.e., `1 + 2` can be written as `(+) 1 2`. The only special thing here is that the operator is encased in parens. And I can mix and match both the styles, e.g., `(1+) 2`. But this is not operator sections. Operator sections leverage this mixed style of invoking the operator functions by creating partials on operator functions. So if I want to define a function that halves its input I can define it is a partial on the division operator -

```haskell
-- defining without partials
f x = x/2

-- defining with partials aka operator sections
g = (/2)
```

## Precedence and `$` operator

Function application have the highest precedence. `sqrt 3 + 4 + 9` is going to take the square root of 3 first, and then add it to 4 and 9.  If I wanted to add the numbers first and then take their square root I’d have to use parens like `sqrt (3 + 4 + 9)`. This can be simplified by using the `$` operator - `sqrt $ 3 + 4 + 9`.  Here is another example. `add 2 3 * 4` will first add 2 and 3 and the multiply the result by 4 giving 20. If I wanted to multiply 3 and 4 first and then add it to 2, I’d have to use parens like `add 2 (3 * 4)`. This can be re-written as `add 2 $ 3 * 4`. Anything to the left of `$` is a function. Anything to the right of the `$` is an expression that will evaluate to some value wich will be the argument of the function on the left.

One final example. The expression below takes in a list of numbers 2 to 10, multiplies each by 2 and then chooses those that are greater than 10. Finally sums up all the chosen numbers. 

```haskell
sum (filter (> 10) (map (*2) [2..10]))
```

This can be re-written as 

```haskell
sum $ filter (> 10) $ map (*2) [2..10]
```

In other words `$` is right associative whereas space is left associative, i.e., `f a b c` is equivalent to `(((f a) b) c)`, i.e., first we apply `f` to `a`, the result should be another function that is applied to `b`, whose result is yet another function that is finally applied to `c` to return the final value. OTOH `f $ a $ b $ c` is equivalent to `f (a (b c))`, i.e, b is a function that is first applied to `c`, its result is a value that is passed into the function `a`, and finally the result of `a` is fed to `f` to get the final answer.

## Function Composition

This can be thought of as a more special case of the `$` operator in that it only works with functions. In math, we have function compositions like $(f \circ g)(x) = f(g(x))$. Haskell has a similar operator `(f . g) x` is equivalent to `f (g x)`. This is also right associative. So if I have three functions `f`, `a`, and `b` and a value `c` then `f $ a $ b $ c` is the same as `(f . a . b) c`.  Example -

```haskell
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
```

This is a function that sums up all numbers less than 10,000 that are squares of odd numbers. The way to write this in function composition is

```haskell
oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
```

But it can be argued that readability takes a hit, so a more readable implementation is -

```haskell
oddSquareSum :: Integer  
oddSquareSum =   
  let oddSquares = filter odd $ map (^2) [1..]  
      belowLimit = takeWhile (<10000) oddSquares  
  in  sum belowLimit  
```





