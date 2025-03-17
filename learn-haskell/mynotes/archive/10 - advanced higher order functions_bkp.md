# Advanced Higher Order Functions

## Summary

A contextualized data type (my term not a formal term) is like a container, it has a raw value but wrapped inside some context. E.g., `List`, `Maybe`, etc. are such data types. Obviously these are parameterized data types, which means that they can wrap different raw types, e.g., List can be a list of integers, a list of strings, etc. Now consider two functions -

* $f1$ is a “normal” function that takes in a (raw) value and returns another (raw) value.
* $f2$ is a function that takes in a raw value and returns a contextualized value, e.g., it might take in a `String` and return a `Maybe Int`.

A contextualized data type is a functor (ok an instance of the Functor typeclass) if it can take $f1$ and apply it to raw value(s) contained inside its context. It then wraps the raw output values in the same context. E.g., if I have a list of integers, I can take in a `addTwo` function and output another list of integers with 2 added to everything. A contextualized data type is a monad if it can take in $f2$ and apply it to raw value(s) contained inside its context and just returns the contextualized value outputted by $f2$ without any further wrapping. E.g., if I have a `Maybe String` then I can take in a function with input `String` and output `Maybe Int`, and apply it to the string contained inside the context. Finally an applicative functor is a contextualized data type if it can take in a $f1$ that is itself wrapped in a context and apply it to the raw values that are wrapped inside it to produce output that is also wrapped in the same context.

```haskell
fmap :: (a -> b) -> t a -> t b
>>= :: t a -> (a -> t b) -> tb
(<*>) :: t (a -> b) -> t a -> t b

f1 :: a -> b
f2 :: a -> t b
```



## Functors

`Functor` is a parameterized typeclass which means that it is a defined for parameterized types aka type ctors.

```haskell
class Functor t where
  fmap :: (a -> b) -> t a -> t b
```

This typeclass is specified for a type constructor that can be used to construct any concrete type. The behavior of this typeclass is specified by just one higher order function - `fmap` that takes in any function that maps any type to some other type, and a concrete type constructed by the type constructor, and returns another concrete type constructed by the same type constructor.

The easiest implementation is for `map`. 

```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap mapper [] = []
myMap mapper xs = [mapper x | x <- xs]
```

Now I can use `myMap` as -

```haskell
> myMap (+ 2) [1, 2, 3]
[3,4,5]

> myMap show [4, 5, 6]
["4","5","6"]
```

I can redefine `myMap` as a `Functor` on `[]` which is a type constructor as opposed to its concrete value constructors `[a]` or `[Int]` and so on.

```haskell
instance Functor [] where
  fmap f [] = []
  fmap f xs = [f x | x <- xs]
```

Similar usage -

```haskell
> fmap (+ 2) [1, 2, 3]
[3, 4, 5]
```

(Data) Types that look like containers can be functors, because they can always define `fmap` s.t a function passed to `fmap` is applied to each element in the container. A more smarter sounding way to describe is that functors can applied to any **computational context**.

```haskell
> :k Functor
Functor :: (* -> *) -> Constraint
```

As can be seen, functors are defined for parameterized types that accept a single concrete type as its parameter. So `Maybe` or `Node` (from my implementation in chapter 4) can be made as functors. But a parameterized type like `Either` cannot be, because it takes two concrete types as parameters.

```haskell
> :k Either
Either :: * -> * -> *

> :k Node
Node :: * -> *

> :k Maybe
Maybe :: * -> *
```

```haskell
data Node a = Empty | Node {val :: a, left :: Node a, right :: Node a} deriving (Show)

instance Functor Node where
  fmap func Empty = Empty
  fmap func (Node v l r) = Node {val = func v, left = fmap func l, right = fmap func r}
```

Of course I can do something like -

```haskell
instance Functor (Either a) where
  fmap func (Right b) = Right (func b)
```

### Lifting a function

```haskell
> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

I think of this signature in two ways - one is that `fmap` takes in two parameters, the first one is a function that takes a single parameter of type a and returns a value of type b, and the second parameter of `fmap` is a functor over type a. The output of `fmap` is another functor over type b, i.e., `((a -> b) -> f a) -> fb`. Another way of looking at this is that `fmap` takes in a single function that takes in a parameter of type a and returns a value of type b, and returns a function that takes in a functor over type a and returns another functor over type b, i.e., `(a -> b) -> (f a -> f b)` 

```haskell
import qualified Data.Char as C

> let letters = ['A', 'P', 'T', 'G']

-- normally I'd do something like this
> fmap C.ord letters
[65,80,84,71]

-- I can lift ord like so. Now ascii will work on any functor.
> ascii = fmap C.ord

> ascii letters
[65,80,84,71]

> let nA = Node 'A' Empty Empty
> let nB = Node 'B' Empty Empty
> let nC = Node 'C' nA nB
> ascii nC
Node {val = 67, left = Node {val = 65, left = Empty, right = Empty}, right = Node {val = 66, left = Empty, right = Empty}}

-- ascii is a function that takes in a functor over Char and returns a functor over Int.
> :t ascii
ascii :: Functor f => f Char -> f Int
```

### Functor Laws

#### First Law

Applying `fmap` with the `id` function on any functor should be the same as applying the `id` function directly to that functor.

```haskell
> id [1, 2, 3]
[1,2,3]

> fmap id [1, 2, 3]
[1,2,3]
```

#### Second Law

Applying `fmap` with a composed function on any functor should be the same as first mapping one function on the functor and then mapping the second function on the resulting functor. 

```haskell
fmap (f . g) = fmap f . fmap g

> fmap (+2) vals
[3,4,5]
> fmap (*3) [3, 4, 5]
[9,12,15]

> fmap ( (*3) . (+2) ) vals
[9,12,15]
```

## Applicative Functors

This typeclass is defined inside `Control.Applicative` and has two functions that define its behavior - 

```haskell
class (Functor t) => Applicative t where
  pure :: a -> t a
  (<*>) :: t (a -> b) -> t a -> t b
```

While normal functors are data types that let a function be applied to each value contained inside them, applicative functors let each function inside them be applied to each value inside them. E.g., lets consider the `Maybe` applicative functor. Lets say we have a function inside it `Just (+2)` which can be applied to `Just 3`. `fmap` will not be able to extract the function out of `Just` and apply it to the value inside another `Just`.

```haskell
> Just (+2) <*> Just 3
Just 5
```

`pure` creates the smallest possible computational context out of the raw value that is passed to it. So passing `pure (+3)` (in context of other `Maybe`s) will produce a `Just (+3)`. I can write the above as -

```haskell
> pure (+2) <*> Just 3
Just 5
```

Another cool thing is that if I have a function that takes in two parameters, I can just chain `<*>` together -

```haskell
> pure (+) <*> Just 2 <*> Just 3
Just 5
```

Different applicative functors behave differently -

```haskell
> pure (+) <*> [1, 2, 3] <*> [10, 20, 30]
[11,21,31,12,22,32,13,23,33]
```

Here `List` is an applicative functor that does a sort of cross-product. The above is the same as writing -

```haskell
> fmap (+) [1, 2, 3] <*> [10, 20, 30]
[11,21,31,12,22,32,13,23,33]
```

Haskell has some syntactic sugar for this pattern, instead of calling `pure` or `fmap`, I can use the `<$>` operator -

```haskell
> (+) <$> [1, 2, 3] <*> [10, 20, 30]
[11,21,31,12,22,32,13,23,33]
```

In general if I have a function `fn` that takes in `n` parameters I can do the following -

```haskell
> fn <$> [x1, y1, z1] <$> [x2, y2, z2] <$> ... <$> [xn, yn, zn]
```

The way `List` implements the applicative functor behavior, this will give me a cross product of $3^n$ values. And of course this is true for any other functor over values as well.

And then I can do a bunch of other fun things as well -

```haskell
> [(+),(*)] <*> [1,2] <*> [3,4]  
[4,5,5,6,3,4,6,8]
```

Here are the intermediate steps -

```haskell
[(+), (*)] <*> [1, 2]
= [(1+), (2+), (1*), (2*)]

[(1+), (2+), (1*), (2*)] <*> [3, 4]
= [(1+3), (1+4), (2+3), (2+4), (1*3), (1*4), (2*3), (2*4)]
= [4, 5, 5, 6, 3, 4, 6, 8]
```

### ZipList

As seen above, `List` as an applicative functor is akin to a tensor product of vectors (or cartesian/cross product of sets). `ZipList` is akin to the dot product.

```haskell
> (*) <$> [1, 2] <*> [3, 4]
[3,4,6,8]

> (*) <$> ZipList [1, 2] <*> ZipList [3, 4]
ZipList {getZipList = [3,8]}
```

Another example -

```haskell
> ZipList [(+), (*)] <*> ZipList [1, 2] <*> ZipList [3, 4]
ZipList {getZipList = [4,8]}
```

## Monads

Monads are data types that can take in a function that takes in a raw value and returns a contextualized output, and apply it to the raw values contained within it.

```haskell
class Monad t where
  return :: a -> t a
  
  (>>=) :: t a -> (a -> t b) -> t b
  
  (>>) :: t a -> t b -> t b
  x >> y :: x >>= \_ -> y
  
  fail :: String -> t a
  fail msg = error msg
```

To understand these functions lets see how `Maybe` becomes a monad.

```haskell
instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x
  fail _ = Nothing
```

`return` is very similar to `pure`. It takes in a value and wraps it in a context.

```haskell
> return "avilay" :: Maybe String
Just "avilay"

> pure "avilay" :: Maybe String
Just "avilay"
```

The `>>=` is the main function and it is called “bind”. As can be seen from the implementation, if `Nothing` is on the LHS then the result is `Nothing`, if the LHS is s.t that the given function returns `Nothing`, then too the result is `Nothing`. Otherwise the result of `f` is wrapped in a `Just`.

```haskell
> func x = if x > 10 then Just (x + 10) else Nothing
> :t func
func :: (Ord a, Num a) => a -> Maybe a

> Nothing >>= func
Nothing

> Just 5 >>= func
Nothing

> Just 12 >>= func
Just 22
```

`>>` is funny function that calls bind with a constant function that always returns its (>>‘s) RHS. If `>>` is called with `Nothing` as its RHS, it will return `Nothing`. But if `>>` is called with `Nothing` on its LHS, then it will just call the bind function with `Nothing` on its LHS and as can be seen from the implementation of bind, that too will return `Nothing`.

```haskell
> Just 4 >> Nothing
Nothing

> Nothing >> Just 4
Nothing

> Just 4 >> Just 42
Just 42
```





## Monoids

Any data type that can go with an associative binary function and has at least one value which acts as an identity is a Monoid.
$$
(x \oplus y) \oplus z = x \oplus (y \oplus z) \\
x \oplus i = i \\
i \oplus x = x \\
x, y, i \in T
$$
$T$ is a data type which has some binary function $\oplus$ that is associative and has an identity value $i$. 

```haskell
class Monoid m where  
  mempty :: m  
  mappend :: m -> m -> m  
  mconcat :: [m] -> m  
  mconcat = foldr mappend mempty
```

Here `mempty` is a polymorphic constant that plays the role of $i$. `mappend` is $\oplus$, even though it has *append* in its name, it is really an accumulator or sorts.

TODO: Monoids are useful when I want to fold any parameterized type, not just lists.

