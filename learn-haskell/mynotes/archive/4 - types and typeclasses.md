# Types and Typeclasses

## Overview

Any entity or expression in a programming language belongs to a some type. Type membership indicates the kind of data that this entity can carry, as well as the behavior it exhibits. E.g., the number 2 is associated with the `Integral` type. And it can be added, subtracted, etc. A function like `add` is also associated with a type, its type is a function that takes in two numbers and returns another number.  In OOP both the data and its behavior is implemented in a class. In Haskell, these are separated out. Types with only data associated with them, like structs, are called **types**, whereas types with only behavior associated with them, like interfaces, are called **typeclasses**. 

## Basic Types

### Data Types

If the result of evaluating an expression is a value whose type is T then it is written as `expr :: T`.  Some basic types are -

| Type         | Description                                                  |
| ------------ | ------------------------------------------------------------ |
| `Bool`       | Only has two values `True` and `False`.                      |
| `Char`       | Single characters.                                           |
| `String`     | These are just `List` of `Char`s. So all functions that are available for lists are available for strings as well. |
| `Int`        | -                                                            |
| `Float`      | -                                                            |
| `List`       | A sequence containing values of the **same** type (unlike Python). This is written as `a :: [Char]` which means that `a` is a list of chars. |
| `Tuple`      | A sequence containing values of **different** types (again unlike Python). This is written as `a :: [Bool, Int, Float]`, which means that `a` is a tuple which has exactly three elements, the first of which is a boolean, etc. |
| `Maybe a`    | This is an example of a parameterized data type explained below. It can be `Nothing` or `Just a`. This is better explained later. |
| `Either a b` | This is another parameterized type which can be either `Left a` or `Right b`. |

To examine the type of an expression or a function just do -

```haskell
> :t "Avilay"
"Avilay" :: [Char]

add :: Int -> Int -> Int
> :t add
...
```

To examine information about the data type itself do -

```haskell
:info Int
type Int :: *
data Int = GHC.Types.I# GHC.Prim.Int#
  	-- Defined in ‘GHC.Types’
:::
```

### Typeclasses

Typeclasses have have bunch of behavior associated with them, e.g., the `Eq` type classes has two functions defined on it, the `(==)` and the `(/=)` functions.  The `Num` typeclass has a number of functions defined on it like `(+)`, `(-)`, `(*)`, `negate`, `abs`, `signum`, `fromInteger`. Typeclasses show up in function type signatures a lot, e.g.,

```haskell
> :t (+)
(+) :: Num a => a -> a -> a
```

What this is saying is that the `+` function takes in two parameters or any type `a` and returns a value of the same type. The only constraint on `a` is that it should belong to the typeclass `Num`, i.e., all the `Num` functions should be defined for this type. `Int` is just such a type. 

```haskell
> :info Int
type Int :: *
data Int = GHC.Types.I# GHC.Prim.Int#
::::
instance Num Int -- Defined in ‘GHC.Num’
:::
```

Here we see that `Int` is an instance of `Num` typeclass. All it means that there is some Haskell module somewhere that has defined the 7 `Num` functions with `Int` as parameters. Here are some common typeclasses -

| Typeclass  | Description                                                  | Behavior                                              |
| ---------- | ------------------------------------------------------------ | ----------------------------------------------------- |
| `Eq`       | Types whose equality can be checked.                         | `(==)`, `(/=)`                                        |
| `Ord`      | Types that can be ordered from highest to lowest.            | `(>)`, `(<)`, `(<=)`, `(>=)`, `max`, `min`, `compare` |
| `Show`     | Types that can be printed to stdout.                         | `showsPrec`, `show`, `showList`                       |
| `Read`     | Types that can be serliazed from String.                     | `read`                                                |
| `Enum`     | Types that can be sequentially ordered. The main advantage is that they can be used in `List` ranges, e.g., `[LT..GT]`. | `succ`, `pred`, etc.                                  |
| `Bounded`  | Types that have an upper and lower bound. e.g,. `minBound :: Int`. | `minBound`, `maxBound`                                |
| `Num`      | Types that are numerical.                                    |                                                       |
| `Integral` | Types that are integers.                                     |                                                       |
| `Integer`  | Types that are integers. `Integer` is a subtype of `Integral` and `Int` is an instance of `Integral`. Dunno how all of this works. |                                                       |
| `Floating` | Floats.                                                      |                                                       |

Just like data types, to get more information about typeclasses also do -

```haskell
> :info Num
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  ::::
```

### Function Types

`a :: Bool -> Bool` is a function that takes in a boolean and returns a boolean. Strictly speaking a function can only take in a single argument. Of course this single argument can be a tuple or a list -

```haskell
add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto x = [0..x]
```

#### Curried Functions

However, we do see functions with multiple arguments, so what gives? These are functions that take in single arugment and return a function which in turn takes in a single argument and so on.

```haskell
add :: Int -> (Int -> Int)
add x y = x + y
```

Here `add` is taking an integer and returning a function that in turn takes an integer and returns another integer. More concretely `add` takes an integer `x` and returns a function that takes an integer `y` and adds it to the closure of `x`. In Python it would be implemented as -

```python
def add(x):
  def add_(y):
    return x + y
  return add_

add(2)(3)
5
```

Another example of curried function with three arguments - 

```haskell
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z
```

`mult x y z` is actually `((mult x) y) z`

In Python -

```python
def mult(x):
  def mult_(y):
    def _mult_(z):
      return x * y * z
    return _mult_
  return mult_

mult(2)(3)(4)
24
```

The rationale for curried functions is not super clear to me, but it seems that it makes using partials easier. For the type annotation, we don't have to use so many parens with the arrows. E.g., `Int -> (Int -> (Int -> Int))` can be written simply as `Int -> Int -> Int -> Int` with the assumption that arrows are grouped from right to left.

#### Polymorphic Functions

This is just a fancy name for templated functions that can take in generic types. E.g., `length :: [a] -> Int` is a function that takes in a `List` of **any** type and returns an `Int`. Another example `zip :: [a] -> [b] -> [(a,b)]`. I can constrain the parameters of such functions (input or output) to have to belong to some typeclass. For example an add function can have the type `add :: a -> a -> a`, but this is too broad. This function only works on numbers, so I can constraint it to accept any generic data type as long as it is a number like this - `add :: Num a => a -> a -> a`.  If I have a generic with two types, I can specify a constraint on either one or both of them.


## Polymorphic Constants

There are some expressions or functions that don’t have a specific type or typeclass. Their type is anything that belongs to some other typeclass. E.g., 

```haskell
> :t 10
10 :: Num p => p
```

This is saying that the literal `10` does not have a concrete type. It can be any type that implements the behavior of the `Num` typeclass. Functions can also be polymorphic constants, e.g., `minBound` - 

```haskell
> :t minBound
minBound :: Bounded a => a
```

Here we can see that `minBound` isn’t really a function, it can be any type that implements the `Bounded` interface aka typeclass. This is why we cannot call `minBound` with any actual parameters. We have to specify the type of `minBound` and that will print out the actual literal min bound value.

```haskell
> minBound :: Char
'\NUL'

> minBound :: Int
-9223372036854775808
```

## Custom (Data) Types

The simplest possible thing to do is -

```haskell
data Shape
  = Circle Float
  | Rectangle Float Float
```

`Shape` is the type, `Circle` and `Rectangle` are called its **value constructors** and they do double duty  -

* Their main job is to act as constructor **functions** to create new shapes

  ```haskell
  > :t Circle
  Circle :: Float -> Shape
  
  > unitCircle = Circle 1
  > unitSquare = Rectangle 1 1
  ```

  This shows that `Circle` is a function that takes in a `Float` and returns a `Shape`.

* Provide the syntax for passing in shapes as input args to functions
  ```haskell
  area :: Shape -> Float
  area (Circle r) = 3.14 * r * r
  area (Rectangle h w) = h * w
  ```

  If I have nested data type then the fields will need to bound recursively in functions.
  ```haskell
  data Point = Point Float Float
  data Shape = Circle Point Float  -- center position, radius
             | Rectangle Point Point  -- upper left, lower right
             
  area :: Shape -> Float
  area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y2)
  ```

> <span style="color:darkorange">It is not possible to “derive” a data type from another data type.</span>

### Data type with a single constructor

```haskell
data Point = Point Float Float
```

The data type and the constructor function both have the same name. Not really required, but is a Haskell convention.

### Associating behavior with data types

When a data type is an **instance** of a typeclass, it means that there are functions of that typeclass implemented for that data type. E.g, lets consider the `Eq` typeclass, which needs one of two functions to be implemented - `(==)` or`(/=)`. Both are not required to be implemented because when `Eq` was defined, both were implemented in terms of another. This was an optional implementation of the typeclass behavior that can be done to make the lives of other data types easier. This can also be seen in the detailed info of `Eq`, the `MINIMAL` comment tells us that `(==)` OR `(/=)` needs to be implemented.

```haskell
:info Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

Here is how we can implement the `Eq` behavior for `Point` by making `Point` an **instance** of `Eq`.

```haskell
instance Eq Point where
  (Point x1 y1) == (Point x2 y2) = x1 == x2 && y1 == y2
```

### Deriving typeclasses

Instead of implementing a typeclass as above, there are certain special typeclasses that can be **derived** from. These are - 

`Eq`, `Ord`, `Enum`, `Bounded`, `Read`, and `Show`. More information is in [Chapter 11 Specification of Derived Instances](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html). So instead of defining `Point` as an instance of `Eq` I could’ve just defined `Point` like this -

```haskell
data Point = Point Float Float deriving (Eq)
```

Now `Point` automagically gets the `(==)` and `(/=)` implementations. Not all typeclasses can be derived from like this.

#### Enabling data types to be printed
```haskell
data Shape
  = Circle Float
  | Rectangle Float Float
  deriving (Show)
```

Now if I do 

```haskell
> unitCircle = Circle 1
> unitCircle
Circle 1.0
```

### Record Types

By assigning field names, Haskell will create functions with the same name as the field that will return the value of that field. It also makes it easy to create and use the data type.

```haskell
data Point = Point {x :: Float, y :: Float}          
```

Notice the use of commas when declaring the constructor this way. Now I can do -

```haskell
> origin = Point {x=0, y=0}
> x origin
0.0
```

```haskell
data Shape = Circle {center :: Point, radius :: Float} 
           | Rectangle {upperLeft :: Point, lowerRight :: Point}
           deriving (Show)
```

Now I can do -

```haskell
> unitSquare = Rectangle {upperLeft=Point {x=0, y=1}, lowerRight=Point {x=1, y=0}}
> x (upperLeft unitSquare)
0.0
```

When defining functions - 

```haskell
area :: Shape -> Float
area (Circle _ r) = 3.14 * r * r
area (Rectangle p1 p2) = abs (x p1 - x p2) * abs (y p1 - y p2)
```

Field names did not come in handy when shapes are passed in as input args to a function, e.g., implementation of area of a circle does not use fields. Even for the area of a rectangle, fields are used inside the function, but in the input args I still have to pass in rectangle without using any field names.

### Nullary data types

These are data types whose constructors do not take any parameters.

```haskell
data Answer = Yes | No | Unknown

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Uknown = Unknown
```

### New types

Data types with exactly 1 constructor with exactly 1 field in it can be declared with the `newtype` keyword. `data` is also valid but there are a bunch of reasons mentioned [here](https://wiki.haskell.org/Newtype) that I did not quite get.

```haskell
newtype Node = Node Float  -- value

newtype Node = Node {value :: Float}

n1 = Node 3.14
n2 = Node 2.71
```

### Parameterized Data Types

```haskell
newtype Node a = Node a
n1 = Node 10
n2 = Node 'A'
```

Previously `Node` could only take in float values, but now it can take in any type of value.

```haskell
foo (Node v) = "This node contains the value " ++ show v
```

Will work for all types of node values as long as they can be `show`n. The closest OO approximation of this concept are templated classes. Here is what an alternative implementation of `Maybe` might look like - 

```haskell
data Optional a = None | ValueOf a deriving (Show)
```

Here is how I’d use it in functions - 

```haskell
safeMult :: Num p => p -> Optional p -> p
safeMult _ None = 0
safeMult x (ValueOf y) = x * y

safeDivide :: (Eq a, Fractional a) => a -> a -> Optional a
safeDivide _ 0 = None
safeDivide x y = ValueOf (x / y)
```

Here `Optional` is a **type constructor**. It can be `Optional Int` or `Optional String` or any other **concrete type**. `None` and `ValueOf Int` would be the corresponding constructors of these concrete types. `ValueOf` just by itself is an `Optional` constructor.

### Recursive Data Types

```haskell
> data MyList a = Empty | Shift {newElem :: a, currList :: MyList a} deriving (Show, Eq, Ord, Read)
> Shift 'B' (Shift 'A' Empty)
Shift {newElem = 'B', currList = Shift {newElem = 'A', currList = Empty}}
```

### Type Synonyms

Just like `typedef` in C++, I can create aliases for any `type` in Haskell for semantic clarity, e.g., `String` is actually just an alias for `[Char]`.

```haskell
> :info String
type String :: *
type String = [Char]
  	-- Defined in ‘GHC.Base’
```

Type synonyms can also be parameterized, either fully like `AssocList` or partially like `IntMap` -

```haskell
type AssocList k v = [(k,v)]
type IntMap v = Map Int v
```

## Custom Typeclasses

### Basics

Here is custom typeclass a.k.a an interface where I am just specifying the function signature. There is no implementaiton provided nor is it neccessary.

```haskell
class Player a where
  play :: a -> String -> [Float]
```

Now I can have two data types like so (remember it is not possible to derive a data type from another data type, so I cannot make `VideoPlayer` a child of `AudioPlayer`).

```haskell
newtype AudioPlayer = AudioPlayer {audioVolume :: Int}

data VideoPlayer = VideoPlayer {videoVolume :: Int, videoBrightness :: Int}
```

And then implement the `Player` interface like so -

```haskell
instance Player AudioPlayer where
  play player song = [start .. start + 5]
    where
      len = fromIntegral (length song)
      vol = fromIntegral (audioVolume player)
      start = len * vol

instance Player VideoPlayer where
  play player video = [start .. start + 10]
    where
      len = fromIntegral (length video)
      vol = fromIntegral (videoVolume player)
      intensity = fromIntegral (videoBrightness player)
      start = len * vol * intensity
```

And now I can use it like so -

```haskell
> ap = AudioPlayer {audioVolume = 10}
> vp = VideoPlayer {videoVolume = 5, videoBrightness = 5}

> play ap "Hello from the other side"
[250.0,251.0,252.0,253.0,254.0,255.0]

> play vp "Blade Runner II"
[375.0,376.0,377.0,378.0,379.0,380.0,381.0,382.0,383.0,384.0,385.0]
```

The `Player` typeclass is defined for some concrete data type `a`, it cannot be defined for something like `Maybe` because `Maybe` is not a concrete type, it can be defined for something like `Maybe a` or `Maybe Int`, etc.

### Default Implementations

Just like with abstract base classes in Pyton, typeclass functions can have default implementations that their instances can choose to override or not. Here is how the `Eq` typeclass defined -

```haskell
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)
```

Both the functions are defined in terms of one another, which means that a data type instance needs to implement just one of them to get the full behavior of `Eq`.

### Subclassing

I can define another interface but mandate that any type that implements this interface must also implement some other interface. E.g., let us define another interface called `HiDefPlayer` but mandate that any instance must already be an instance of `Player`, because we expect to use the `play` function in the behavior of `HiDefPlayer` .

```haskell
class (Player a) => HiDefPlayer a where
  playInHiDef :: a -> String -> [Float]
```

Now I can create a new data type called `HiDefAudioPlayer` and have it implement both the interfaces.

```haskell
data HiDefAudioPlayer = HiDefAudioPlayer {hiDefAudioVolume :: Int, audioQualitySetting :: Int}

instance Player HiDefAudioPlayer where
  play player song = [start .. start + 10]
    where
      len = fromIntegral (length song)
      vol = fromIntegral (hiDefAudioVolume player)
      start = len * vol

instance HiDefPlayer HiDefAudioPlayer where
  playInHiDef player song = [byte + qual | byte <- stream]
    where
      qual = fromIntegral (audioQualitySetting player)
      stream = play player song
```

### Parameterized Instances

So far we have seen that the instance types are simple concrete types like `AudioPlayer`. However, the instances can also be parameterized data types. E.g., recall our parameterized data type `Node a`, the equality behavior can be implemented as follows:

```haskell
data Node a = Empty | Node {val :: a, left :: Node a, right :: Node a} deriving (Show)

instance (Eq a) => Eq (Node a) where
  Empty == Empty = True
  Empty == _ = False
  _ == Empty = False
  n1 == n2 = val n1 == val n2 && left n1 == left n2 && right n1 == right n2
```

Here instead of saying `instance Eq Node` I have to say at the very least `instance Eq (Node n)` because `Node` is not a concrete type. It is just a type constructor. But `Node n` is a concrete type. Now in the implementation of the `==` operator, I will need to check the equality of the `val` field, so I have to specify an additional constraint that the parameter of `Node` must implement `Eq` as well. See how the syntax for this constraint specification and the base class is the same. Both are just specifying constraints on different parts of the concrete type of the instance.

### Parameterized Typclasses

In the previous example I had to specify the instance of `Eq` with a concrete type instead, i.e., `Node a` instead of a type constructor, i.e., `Node` because that is how `Eq` is defined. I can ofcourse define a typeclass that takes in a type constructor instead of a concrete type like so -

```haskell
class TreeTraverser t where
  postOrder :: t a -> [a]
  preOrder :: t a -> [a]
```

The parameter of the typeclass is still a single entity `t`, but later on in the function signature we see that `t` is actually a type constructor. `Node` can be an instance of this like so -

```haskell
	instance TreeTraverser Node where
  postOrder Empty = []
  postOrder n = val n : postOrder (left n) ++ postOrder (right n)
  preOrder Empty = []
  preOrder n = preOrder (left n) ++ [val n] ++ preOrder (right n)
```

Now I can call these traversal methods with any concrete type of node -

```haskell
> n = Node 10 (Node 20 Empty Empty) (Node 30 Empty Empty)
> postOrder n
[10,20,30]
> preOrder n
[20,10,30]

> m = Node 'A' (Node 'B' (Node 'C' Empty Empty) Empty) Empty
> postOrder m
"ABC"
> preOrder m
"CBA"
```

## Kinds

Just like values like `‘A’, 2, add` have types associated with them, types (both data types and typeclasses) have **kinds** associated with them. To see the kind of a simple data type do

```haskell
> :k Int
Int :: *
```

This tells us that the data type `Int` is of a **concrete** type as indicated by the `*`. To see the kind of a type constructor of a parameterized data type do the same thing -

```haskell
> :k Maybe
Maybe :: * -> *
```

But now we see that `Maybe` is like a function that takes in a concrete type and returns another concrete type. And finally to see the kind of a typeclass do the same thing again -

```haskell
> :k Num
Num :: * -> Constraint
```

Here we see that `Num` is also function-like in that it takes a concrete type and returns back a constraint.



