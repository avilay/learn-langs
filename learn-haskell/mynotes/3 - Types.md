# Types

## Even Shorter Summary

```haskell
-- newtype
newtype ModelId = ModelId Int
newtype Health = Health { remainingHealth :: Int }

-- record type
data Point = Point {x :: Integer, y :: Integer}

-- type constructor, e.g., Maybe, Either
data Node a = EmptyNode | Node {value :: a, next :: Node a}

-- concrete typeclass, e.g., Eq, Num, Ord, Show
class Distinct a where
  distance :: a -> a -> Float
  
-- use it as a constraint
speed :: (Distinct a) => a -> a -> Float -> Float
speed x0 x1 t = distance x1 x0 / t

-- concrete kinds can be instances of this typeclass
instance Distinct String where
  distance str1 str2 = ...
  
instance Distinct Point where
  distance point1 point2 = ...

-- implemented for concrete kind (Node a)
instance Distinct (Node a) where
  distance node1 node2 = ...

-- instances can have constraints of their own
instance Eq a => Distinct (Node a) where
  distance (Node val1 next1) (Node val2 next2) = ...

-- typeclasses can have also have constraints called "superclass"
class (Eq a) => Sequence a where
  cardinality :: a -> Int
  
-- typeclass for type constructors  
class Singleton f where
	singleton :: a -> f a

-- implemented for type constructor kind Node
instance Singleton Node where
	singleton val = Node val EmptyNode  
```

## Summary

```haskell
-- TYPE
x = True
:t x
> x :: Bool

-- KIND
:k Bool
> Bool :: *

-- NEWTYPE
newtype ModelId = ModelId Int
newtype Health = Health { remainingHealth :: Int }

-- DATA TYPE
data Point = Point {x :: Integer, y :: Integer}
origin = Point 0 0

-- type of origin is Point
:t origin
> origin :: Point

-- kind of Point is a concrete type *
:k Point
> Point :: *

-- PARAMETRIC DATA TYPE aka TYPE CONSTRUCTORS
data Node a = EmptyNode | Node {value :: a, next :: Node a}
head = Node 'A' (Node 'B' EmptyNode)

-- type of head is (Node Char)
:t head
> head :: Node Char

-- kind of (Node Char) is a concrete type *
:k Node Char
> Node Char :: *

-- kind of Node is a type constructor that takes a concrete type
-- to another concrete type
:k Node
> Node :: * -> *

-- Maybe and Either are commonly used built-in parameteric types

-- PARAMETERIZED POLYMORPHIC FUNCTION
reverse :: [a] -> [a]
reverse list = ...

-- TYPECLASS FOR CONCRETE TYPES
-- typeclass is always parametric with a concrete type as the param
class Distinct a where
  distance :: a -> a -> Float

-- kind of a typeclass is a function that takes a concrete type to Constraint.
ghci> :k Distinct
Distinct :: * -> Constraint

-- Acts as constraint on polymorphic functions
speed :: (Distinct a) => a -> a -> Float -> Float
speed x0 x1 t = distance x1 x0 / t

-- Eq, Num, Ord, Show, Read, and Foldable are commonly used builtin typeclass for concrete types
-- Semigroup and Monoid are simple builtin typeclasses for concrete types

-- Concrete data types can implement these typeclasses
instance Distinct String where
  distance str1 str2 = ...
  
instance Distinct Point where
  distance point1 point2 = ...

-- Implementing Distinct for a concrete type (Node a).
instance Distinct (Node a) where
  distance node1 node2 = ...

-- the instances themselves can have constraints
-- I am going to use val1 == val2 in the implementation below
instance Eq a => Distinct (Node a) where
  distance (Node val1 next1) (Node val2 next2) = ...
  
-- typeclasses can have super class, which is just a constraint at the class 
-- definition level
class (Eq a) => Sequence a where
  cardinality :: a -> Int
  
-- Eq is a super class for Ord
-- Semigroup is a super class for Monoid  
ghci> :k Distinct
Distinct :: * -> Constraint
ghci> :k Eq
Eq :: * -> Constraint
ghci> :k Ord
Ord :: * -> Constraint

-- TYPECLASS FOR TYPE CONSTRUCTORS
class Singleton f where
	singleton :: a -> f a

-- kind of this typeclass is a function that takes a type constructor to a Constraint.
ghci> :k Singleton
Singleton :: (* -> *) -> Constraint

-- instance is a type constructor Node, not a concrete type (Node a)
instance Singleton Node where
	singleton val = Node val EmptyNode
  
instance Singleton Maybe where
	singleton val = Just val
  
singleton 10 :: Node Int  
singleton 10 :: Maybe Int

-- Functor is a common typeclass for type constructors
ghci> :k Functor
Functor :: (* -> *) -> Constraint
```

## Types and Kinds

All expressions evaluate to some value that belongs to some **type**. E.g., `x + y` will evaluate to `7` (say), `7` is a **type of** `Integer`, written as `7 :: Integer`. Types in turn belong to a **kind**, e.g., `Integer` is a **kind of** concrete type, represented in Haskell as `*`. Functions being a first class concept in Haskell, they also have a type which is their function signature. E.g., `add x y = x + y` is a type of function that maps a number to a number to a number, i.e, `add :: Num a => a -> a -> a`. The weird `Num a => ` syntax will be clear after learning about ad-hoc polymorphic functions inside typeclasses.

I can find the type of an expression using `:t`  and the kind using `:k` in GHCI -

```haskell
ghci> isActive = True
ghci> :t isActive
isActive :: Bool
ghci> :k Bool
Bool :: *
```

Oftentimes it is useful to use `:info` to get full information on either.

## Concrete Types

### Type Alias

This is same as `typedef` in C. 

```haskell
type ModelId = Int

deploy :: ModelId -> Int -> String
deploy (ModelId modelId) numShards = "Deploying model " ++ show modelId ++ " on " ++ show numShards ++ " shards."

ghci> model = 10
ghci> shards = 3
ghci> deploy model shards
"Deploying model 10 on 3 shards."
```

But there is no compile time checking of this, I can always do this -

```haskell
ghci> deploy shards model
"Deploying model 3 on 10 shards."
```

and it will still compile and run.

### `newtype`s

A slight improvement on type aliases where compile time type checking is available. There are also some performance benefits of using `newtype`.

```haskell
newtype ModelId = ModelId Int deriving (Show)

deploy :: ModelId -> Int -> String
deploy modelId numShards = "Deploying model " ++ show modelId ++ " on " ++ show numShards ++ " shards."
```

```Haskell
-- there is compile time checking 
$ ghc -Wall Main.hs -o ~/temp/bin/scratch
[1 of 2] Compiling Main             ( Main.hs, Main.o ) [Source file changed]

Main.hs:19:31: error:
    • Couldn't match expected type ‘Integer’ with actual type ‘ModelId’
    • In the second argument of ‘deploy’, namely ‘modelId’
      In the expression: deploy numShards modelId
      In an equation for ‘repr’: repr = deploy numShards modelId
   |
19 |       repr = deploy numShards modelId
   |                               ^^^^^^^
```

Ignore the details of `deriving (Show)` for now. Know that it is like getting an automatic implementation of the Python equivalent of `__repr__`, in that it makes this type printable.

There are two ways I can call `deploy` with `ModelId` -

```haskell
-- Instantiating model id beforehand
ghci> mid = ModelId 10
ghci> deploy mid 16
"Deploying model ModelId 10 on 16 shards."

-- Instantiating model id while calling the function
ghci> deploy (ModelId 10) 16
"Deploying model ModelId 10 on 16 shards."
```

With type aliases I could declare a variable like `model = 10` but with newtypes I have to use the type constructor `model = ModelId 10`. The type constructor must have the same name as the type for newtypes. Furthermore, there can only be one property on this type. 

In the above function definition of `deploy`, I did not need to "pull" out the model ID contained inside the `ModelId` datatype so I was able to define the function as I did - `deploy modelId shards = ...`. However if there is a scenario where I do need to pull the contained value out, I'll have to define my function in terms of the constructor.

```haskell
newtype Offset = Offset Int

write :: Int -> Offset -> String -> String
write base (Offset offset) msg = replicate (base + offset) ' ' ++  msg
```

Defining this function as `write base offset msg = ...` will not work. I can make the property a named property in the manner of the so-called *record types* discussed later, which will make it easy to pull out the contained data.

```haskell
newtype ModelId = ModelId {mid :: Int}

deploy :: ModelId -> Int -> String
deploy model numShards = "Deploying model " ++ show (mid model) ++ " on " ++ show numShards ++ " shards."
```

I can now "pull out" the id from the `ModelId` object using this getter like so `mid model` where `model :: ModelId`. This is what I am doing in the function implementation `show (mid model)`.

### Nullary Types

These are like enums in other languages. When a data type has a bunch of mutually exclusive choices, I can use this type.

```haskell
data Color = Red | Green | Blue
```

When declaring this type, I have to use the more general `data` keyword. When using in functions, I can either use pattern-matching or the `case-of` syntax.

```haskell
showColor :: Color -> String
showColor Red = "This is red"
showColor Green = "This is green"
showColor Blue = "This is blue"

displayColor :: Color -> (Int, Int, Int)
displayColor color = case color of
  Red -> (255, 0, 0)
  Green -> (0, 255, 0)
  Blue -> (0, 0, 255)
```

These functions can be called like so -

```haskell
-- Call function with a variable
ghci> favorite = Red
ghci> showColor favorite
"This is red"

-- Call function with a "literal"
ghci> displayColor Red
(255,0,0)
```

### Sum Types

These are types that are composed of other types, but their domain is the sum of the domains of the other types.

```haskell
data Result = Error String | Ok Int
```

Here `Result`'s domain is the sum of the domains of `String` and `Int` because it can be either one of those values but not both at the same time. These can be used in functions like so -

```haskell
addPositives :: Int -> Int -> Result
addPositives x y =
  if x > 0 && y > 0
    then Ok (x + y)
    else Error "Can only add positive numbers!"

-- With pattern matching
showResult :: Result -> String
showResult (Ok value) = show value
showResult (Error msg) = msg

-- With case-of
displayResult :: Result -> String
displayResult result = case result of
  Error msg -> "Error: " ++ msg
  Ok answer -> "Answer: " ++ show answer
```

```haskell
ghci> x = addPositives 10 20
ghci> x
Ok 30
ghci> y = addPositives (-10) 20
ghci> y
Error "Can only add positive numbers!"
ghci> showResult x
"30"
ghci> showResult y
"Can only add positive numbers!"
```

### Product Type

These are also composite types but their domain is the cross product of the domains of the other types. They are very similar to typical user defined data classes in other languages.

```haskell
data User = User String Int Bool

whoami :: User -> String
whoami (User name _ _) = "I am " ++ name
```

And then call this function as -

```haskell
ghci> me = User "Avilay" 100 True
ghci> whoami me
"I am Avilay"
```

#### Record Type

The properties in the `User` type are anonymous. It will be helpful to name them.

```haskell
data User = User {
  userName     :: String,
  userScore    :: Int,
  userIsActive :: Bool
}
```

Now we can "pull" the property value from any user object using the function syntax.

```haskell
whoami :: User -> String
whoami user = "I am " ++ userName user
```

It is a good practice to name the attribute as `userName` instead of just `name` because as it can be seen, these functions are made available in the global namesapce. If there are other data types with the attribute `name`, they can clash.

I can continue to declare the user object without specifying the attribute names or I can use them. Either way works -

```haskell
ghci> me = User "Avilay" 100 True
ghci> anu = User {userName = "Anika", userScore = 1000, userIsActive = True}

ghci> whoami me
"I am Avilay"

ghci> whoami anu
"I am Anika"
```

I can even "clone" a new user object from an existing one with just a few attributes different -

```haskell
aptg = avilay { userName = "aptg" }
-- both avilay and aptg are valid objects now.
```

I can pass in record types in functions in either their "unraveled" form or their "raveled" form -

```haskell
predict user
  | userIsActive user = userScore user * 10
  | otherwise = userScore user `div` 10
  
tally (User userName, userScore, userIsActive)
	| userIsActive = userScore + 10
	| otherwise = userScore `div` 10
```

#### Combination of Sum and Product

I can also have a combination type where the data type is a choice amongst different things but each thing is a product type. In the example below, `Circle` and `Rectangle` **can be considered** product types, but `Shape` is a sum type of `Circle` and `Rectangle`. 

```haskell
data Point = Point {x :: Float, y :: Float} deriving (Show)

data Shape
  = Circle {center :: Point, radius :: Float}
  | Rectangle {upperLeft :: Point, lowerRight :: Point}
  deriving (Show)
```

Both `Circle` and `Rectangle` are known as the constructors. They do double duty -

1. Used in instantiating the object.

```haskell
ghci> origin = Point {x = 0, y = 0}
ghci> unitCircle = Circle origin 1.0
ghci> square = Rectangle {upperLeft = (Point 0 1), lowerRight = (Point 1 0)}
```

2. Used as function arguments.

```haskell
area :: Shape -> Float
area (Circle _ r) = 3.14 * r * r
area (Rectangle ul lr) =
  let length = x lr - x ul
      breadth = y ul - y lr
   in length * breadth

perimeter (Circle _ r) = 2 * 3.14 * r
perimeter (Rectangle (Point upperLeftX upperLeftY) (Point lowerRightX lowerRightY)) =
  let length = lowerRightX - upperLeftX
      breadth = upperLeftY - lowerRightY
   in 2 * (length + breadth)
```

As can be seen in the two definitions, I can "unravel" the type to whatever degree I want.

#### Recursive Data Types

The constructor of a data type can refer to the self type.

```haskell
data BinaryTreeNode = 
	EmptyBinaryTreeNode |
	CreateBinaryTreeNode {
		value :: Int, 
		left :: BinaryTreeNode, 
		right :: BinaryTreeNode
	}
  deriving (Show)
```

As usual, I can unravel the constructor to whatever degree when passing in this type as a function parameter (example is left as an exercise for the reader).

I can use it like so -

```haskell
ghci> four = CreateBinaryTreeNode 4 EmptyBinaryTreeNode EmptyBinaryTreeNode
ghci> seven = CreateBinaryTreeNode 7 EmptyBinaryTreeNode EmptyBinaryTreeNode
ghci> six = CreateBinaryTreeNode 6 four seven
ghci> six
CreateBinaryTreeNode {value = 6, left = CreateBinaryTreeNode {value = 4, left = EmptyBinaryTreeNode, right = EmptyBinaryTreeNode}, right = CreateBinaryTreeNode {value = 7, left = EmptyBinaryTreeNode, right = EmptyBinaryTreeNode}}
ghci> value six
6
ghci> value (left six)
4
ghci> value (right six)
7
ghci> value (left (left six))
*** Exception: No match in record selector value
ghci> left (left six)
EmptyBinaryTreeNode
```

>  The exception in GHCI output above is the reason it makes sense to use the constructor pattern in function parameter.

## Parameteric Data Types

These are like generics in other languages. In the above example the binary tree can only hold integers. We can make it hold any type by making the value type a parameter. Type parameters are usually written as lower case `a`, `b`, etc.

```haskell
data BinaryTreeNode a =
  EmptyBinaryTreeNode |
  CreateBinaryTreeNode {
    value :: a,
    left  :: BinaryTreeNode a,
    right :: BinaryTreeNode a
  }
  deriving (Show)
```

Usually parameteric data types are container-like, i.e., they wrap or contain another as-yet-unspecified type. 

Now, I can instantiate/use binary trees with different types for values -

```haskell
ghci> a = CreateBinaryTreeNode 'A' EmptyBinaryTreeNode EmptyBinaryTreeNode
ghci> c = CreateBinaryTreeNode 'C' EmptyBinaryTreeNode EmptyBinaryTreeNode
ghci> b = CreateBinaryTreeNode 'B' a c
ghci> b
CreateBinaryTreeNode {value = 'B', left = CreateBinaryTreeNode {value = 'A', left = EmptyBinaryTreeNode, right = EmptyBinaryTreeNode}, right = CreateBinaryTreeNode {value = 'C', left = EmptyBinaryTreeNode, right = EmptyBinaryTreeNode}}

ghci> :{
ghci| two = CreateBinaryTreeNode {
ghci|   value = 2,
ghci|   left = CreateBinaryTreeNode {
ghci|     value = 1,
ghci|     left = EmptyBinaryTreeNode,
ghci|     right = EmptyBinaryTreeNode
ghci|   },
ghci|   right = CreateBinaryTreeNode {
ghci|     value = 3,
ghci|     left = EmptyBinaryTreeNode,
ghci|     right = EmptyBinaryTreeNode
ghci|   }
ghci| }
ghci| :}

ghci> true = CreateBinaryTreeNode True EmptyBinaryTreeNode EmptyBinaryTreeNode
```

But I cannot mix-and-match different types -

```haskell
ghci> one = CreateBinaryTreeNode 1 b EmptyBinaryTreeNode

<interactive>:113:28: error:
    • No instance for (Num Char) arising from the literal ‘1’
    • In the first argument of ‘CreateBinaryTreeNode’, namely ‘1’
      In the expression: CreateBinaryTreeNode 1 b EmptyBinaryTreeNode
      In an equation for ‘one’:
          one = CreateBinaryTreeNode 1 b EmptyBinaryTreeNode
```

#### Type Constructors

> Refresh types and kinds: Lets say we have `origin = Point 0 0`, then `origin` is a type of `Point` and `Point` is a kind of `*`, i.e., concrete type.
>
> ```haskell
> ghci> data Point = Point {x :: Float, y :: Float} deriving (Show)
> ghci> origin = Point 0 0
> ghci> :t origin
> origin :: Point
> ghci> :k Point
> Point :: *
> ```

`b` is a type of `BinaryTreeNode Char` and `BinaryTreeNode Char` is a kind of `*`.

```haskell
ghci> :t b
b :: BinaryTreeNode Char
ghci> :k BinaryTreeNode Char
BinaryTreeNode Char :: *
```

But when I try to examine the kind of `BinaryTreeNode` itself, I see something curious -

```haskell
ghci> :k BinaryTreeNode
BinaryTreeNode :: * -> *
```

This is telling me that it is a kind of function that takes in a concrete type and returns a concrete type. This makes sense when we think of `BinaryTreeNode` as a type constructor that takes in `Char` and returns `BinaryTreeNode Char`. Thus `BinaryTreeNode` is a kind of **type constructor**.

Some common type constructors are -

```haskell
ghci> :info Maybe
type Maybe :: * -> *
data Maybe a = Nothing | Just a
  	-- Defined in ‘GHC.Maybe’
```

```haskell
-- Takes in two input types a and b and returns a single input type Either.
ghci> :info Either
type Either :: * -> * -> *
data Either a b = Left a | Right b
  	-- Defined in ‘Data.Either’
```

> In the info display, even though it says `type Maybe :: * -> *` it really means that the **kind** of `Maybe` is `* -> *`. Verify this by typing `:k Maybe` in GHCI.

## Functions

### Parameteric Polymorphism

So far we have defined functions that have a specific concrete type as their input/output parameters. However, it is possible to have functions that take in type parameters and exhibit parameteric polymorphism where it behaves the same way irrespective of the input data type. E.g, `length`, `reverse`, etc. are such functions. It does not matter whether we pass in a list of numbers or characters or booleans, their behavior is the same. Here is an example -

```haskell
rightReverse :: [a] -> [a]
rightReverse list =
  let mid = length list `div` 2
      (left, right) = splitAt mid list
   in left ++ reverse right
```

Just like in parameteric data types, we use lower case  `a`  to represent a concrete type. When `rightReverse` is called, we obviously need to call with a list of concrete types, and the `a` inside the method (had it been used) would've been replaced with the concrete type.

### Type Classes for Concrete Kinds

#### Defining a new typeclass

What if I want the same function to behave differently for different types of inputs? This is the traditional OO polymorphism and is called **ad-hoc polymorphism** in functional languages. This is acheived by defining a typeclass which is a group of related functions that can be implemented by a concrete type to behave in whatever way is needed.

```haskell
class Distinct a where
  distance :: a -> a -> Float
```

Think of this as an interface with a single method. Any concrete data type can implement this interface, i.e., become **an instance of this typeclass** in Haskell lingo.

```haskell
instance Distinct Point where
	distance (Point x1 y1) (Point x2 y2) = 
	  let x = x2 - x1
	      y = y2 - y1
	   in sqrt (x^2 + y^2)
	   
	   
-- implement hamming distance for strings	   
instance Distinct String where
  distance s1 s2 = fromIntegral (go 0 s1 s2)
    where
      go :: Int -> String -> String -> Int
      go acc [] [] = acc
      go acc [] ys = acc + length ys
      go acc xs [] = acc + length xs
      go acc (x : xs) (y : ys) =
        if x /= y
          then go (acc + 1) xs ys
          else go acc xs ys
```

Typeclasses are always defined for a type, which is why it is always of form `class <ClassName> <typevar> where`. Any type that wants to implement this interface aka typeclass will have syntax `instance <ClassName> <TypeName> where`.

When I call the `distance` function, Haskell type inference will infer the concrete instance of distance being called based on the type of the arguments -

```haskell
-- this will invoke the String instance
distance "Avilay" "Anika"

-- this will invoke the Point instance
distance (Point 1 0) (Point 0 1)

-- this will give a compile error because distance is not implemented for Ints
distance 10 20
```

#### Typeclasses as Constraints

Typeclasses do not have a type themselves, they only have kinds. The kind of our newly defined `Distinct` typeclass is `Distinct :: * -> Constraint`. This tells us that it is something that takes a concrete type and makes it into a constraint. This is because typeclasses are generally used to add contraints or upper bounds to function signatures.

Lets say I define a function `speed` that uses `distance`. The type signature of `speed` will indicate that the input type can be anything as long as it is an instance of `Distinct`. 

```haskell
ghci> speed s1 s2 t = (distance s1 s2) / t
ghci> :t speed
speed :: Distinct a => a -> a -> Float -> Float
```

 #### Built-in Typeclasses

`Eq`, `Num`, `Ord`, `Show`, `Read` are commonly used builtin typeclass for concrete types. Lets examine some of them.

##### Eq

```haskell
ghci> :info Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
  	-- Defined in ‘GHC.Classes’
```

First off, the kind of `Eq` has a single `*` on the lhs, which tells us that it is meant for concrete kinds. Second interesting thing to note is the `MINIMAL` pragma which says we don't have to implement both the `(==)` and `(/=)` methods, we can get away with implement any one. This is because the class has default implementations for them - in this case in terms of each other. Here is what the implementation looks like from the Haskell source -

```haskell
class Eq a  where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    x == y = not (x /= y)
    {-# MINIMAL (==) | (/=) #-}
```

##### Num

`Num` is an interesting one. Like in all languages Haskell has a bunch of numeric types like `Int`, `Integer`, `Int64`, `Float`, `Double`, etc. And because it is a statically typed language, converting between these types is a pain. This is why functions will often just add a `Num` constraint on the input type in their type signatures. `Num` in turn has a bunch of usual numerical operations and all different built-in numeric types implement this typeclass.

```haskell
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
```

TODOs

* Default methods in typeclass
*  Automatic generation of instances for some builtin typeclasses using the keyword `deriving`.

### Type Classes for Type Constructors

The concept of type constructors is useful in defining typeclasses that act on type constructors instead of concrete types. Lets say I want to define a function `singleton` for the `BinaryTreeNode` type constructor so that it will create a leaf binary node. And then I want to make this function available to any other type constructor that wants to implement it so they can return their own concrete type, e.g., `Maybe` might implement it to return `Just x`, `Either` might want to return `Right x`, etc. I can define a typeclass for type constructors -

```haskell
class Singleton f where
	singleton :: a -> f a
```

Here `f`'s ~~type~~ kind will automatically be inferred as a type constructor, i.e., `* -> *`. Which makes `Singleton`'s kind as something that takes a type constructor to a constraint.

```haskell
ghci> :k Singleton
Singleton :: (* -> *) -> Constraint
```

```haskell
instance Singleton [] where
  singleton x = [x]

instance Singleton Maybe where
  singleton = Just

instance Singleton (Either e) where
  singleton = Right

instance Singleton BinaryTreeNode where
  singleton x = CreateBinaryTreeNode x EmptyBinaryTreeNode EmptyBinaryTreeNode
```

Unlike concrete typeclasses, Haskell type system cannot infer which instance I am calling in every case, e.g., if I say -

```haskell
-- Which instance to invoke?
singleton 10
```

I need to explicity specify the type constructor I want to use -

```haskell
ghci> singleton 10 :: [Int]
[10]

ghci> singleton 10 :: Maybe Int
Just 10

ghci> singleton 10 :: BinaryTreeNode Int
CreateBinaryTreeNode {value = 10, left = EmptyBinaryTreeNode, right = EmptyBinaryTreeNode}
```

But there can be other typeclass definitions where it is possible to infer the types in question, e.g.,

```haskell
class Functor f where
	fmap :: (a -> b) -> f a -> f b
```

Here I am giving the container aka type constructor as input argument, so Haskell type system can figure out the type/kind of `f`.

`Functor` s are implemented for `Maybe` type constructor, so if I do -

```haskell
ghci> fmap (\x-> 2 * x) (Just 10)
Just 20
```

From the second argument it is clear that I mean to invoke `Maybe` instance of `Functor`. 

I can continue to use this typeclass as a constraint -

```haskell
func :: Singleton a => a -> Float
...
```

> `Either` is special in that I can only implement any typeclasse for only `Right` and not `Left`. Had I defined the singleton method like `singleton = Left` I'd have gotten a compile error.





