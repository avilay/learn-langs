# Modules

## Importing

`import Data.List` is equivalent to saying `from Data.List import *` in Python. In GHCI, the modules are already loaded into the global namespace when I do `import Data.List` so I don’t get all its functions in the global namespace. To do that I have to 

```haskell
> :m + Data.List Data.Map Data.Set
```

Here I am importing all the functions in these three modules in the global namespace.

`import Data.List (nub sort)` is equivalent to `from Data.List import nub, sort`. To avoid name clashes I can keep the namespace. E.g., `Data.Map` has a `filter` and the `Prelude` also has a `filter` which can lead to name clashes -

```haskell
> :m + Data.Map
> filter odd [1..10]
<interactive>:54:1: error:
    Ambiguous occurrence ‘filter’
    :::
```

To avoid this I can do a qualified import or an aliased qualified import 

```haskell
import qualified Data.Map as M
```

This is the same as `import Data.Map as M` in Python.

## `Data.List`

```haskell
import qualified Data.List as L

-- spread a value in between all the elements of a list
> L.intersperse '.' "MONKEY"
"M.O.N.K.E.Y"

-- sperad a list in between elements of a list of lists and then flattens
-- think of this as join strings
> L.intercalate [0,0] [[1, 2, 3], [4, 5], [6, 7, 8]]
[1,2,3,0,0,4,5,0,0,6,7,8]

> L.intercalate " " ["hey","there","guys"]  
"hey there guys"

-- transpose a 2D list
> L.transpose [[1,2,3],[4,5,6],[7,8,9]]  
[[1,4,7],[2,5,8],[3,6,9]]

-- flatten a list
> L.concat [[3,4,5],[2,3,4],[2,1,1]]  
[3,4,5,2,3,4,2,1,1]

-- check if all the elements in a list satisfy a predicate
> L.and $ map (>4) [5,6,7,8]  -- works only on a list of booleans
True
> L.all (>4) [5,6,7,8]  -- first applies the predicate and then applies and

-- check if at least one element in the list satisfies a predicate
> L.or $ map (==4) [2,3,4,5,6,1]  
True
> L.any (==4) [2,3,4,5,6,1]

-- infinite sequence by repeatedly applying a function to the first value 
-- and then its result and so on
> take 10 $ L.iterate (*2) 1  
[1,2,4,8,16,32,64,128,256,512]

-- splits a list into two different size lists
> L.splitAt 3 "heyman"  
("hey","man")

-- select all elements until the predicate is true
-- drop the rest of the list at the first false evaluation
> L.takeWhile (>3) [4, 5, 6, 1, 2, 8, 9, 10]
[4,5,6]

-- opposite of takeWhile, drop all elements until the predicat is true
-- select the rest of the list at the first false evaluation
> L.dropWhile (>3) [4, 5, 6, 1, 2, 8, 9, 10]
[1,2,8,9,10]

-- get two lists in a tuple - 
-- first is the result of takeWhile, i.e., elements that keep passing the predicate
-- second is the result of dropWhile, i.e., elements after the first false evaluation
> L.span (>3) [4, 5, 6, 1, 2, 8, 9, 10]
([4,5,6],[1,2,8,9,10])

-- inverse of span
-- first list contains elements that keep failing the predicate
-- second list contains elements after the first pass evaluation
> L.break (==4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])

-- sort
> L.sort [8,5,3,2,1,6,4,2]  
[1,2,2,3,4,5,6,8] 

-- group adjacent equal elements together
> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

-- Can be used to implement a Counter like class
> mapper = map (\xs@(x:_) -> (x, length xs))
> counter = mapper . L.group . L.sort
> counter [2,1,1,2,5,3,6,2,1,1,2,7]
[(1,4),(2,4),(3,1),(5,1),(6,1),(7,1)]

-- search for a sub list within a list
> "cat" `L.isInfixOf` "im a cat burglar"  
True

-- Python equivalent of startswith and endswith
> "hey" `L.isPrefixOf` "hey there!"  
True

> "there!" `L.isSuffixOf` "oh hey there"  
False 

-- split a list into two lists
-- first one is **all** elements that satisfy a predicate
-- second one is **all** elements that don't
> L.partition (>3) [1,3,5,6,3,2,1,0,3,7]  
([5,6,7],[1,3,3,2,1,0,3])

-- finds the first occurence of a value in a list
> L.find (>4) [1, 6, 3, 4, 9, 10, 4, 2, 3]
Just 6

-- gets the index of the first occurrence of a value if it exists, or all occurrences
> 4 `L.elemIndex` [1, 6, 3, 4, 9, 10, 4, 2, 3]
Just 3
> 4 `L.elemIndices` [1, 6, 3, 4, 9, 10, 4, 2, 3]
[3, 6]
> L.findIndex (>4) [1, 6, 3, 4, 9, 10, 4, 2, 3]
Just 1
> L.findIndices (>4) [1, 6, 3, 4, 9, 10, 4, 2, 3]
[1,4,5]

-- split or join string by newline
> L.lines "first line\nsecond line\nthird line"  
["first line","second line","third line"]  
> L.unlines ["first line", "second line", "third line"]  
"first line\nsecond line\nthird line\n"

-- split or join string by "word" or space
> L.words "hey these are the words in this sentence"  
["hey","these","are","the","words","in","this","sentence"]
> L.unwords ["hey","there","mate"]  
"hey there mate"

-- dedup a list
> L.nub [1,2,3,4,3,2,1,2,3,4,3,2,1]  
[1,2,3,4]

-- delete the first occurence of a value
> L.delete 'h' "hey there ghang!"  
"ey there ghang!"

-- **by** variations - nubBy, deleteBy, groupBy, sortBy, etc. take an additional predicate to
-- compare instead of the default (==) comparison. See sidebar for `on` function
> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]  
> L.groupBy ((==) `on` (> 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

### `on` function

Imported from `Data.Function`, the `on` functions takes in two functions `f` and `g` and returns a new function that applies them like this - `\x y -> f (g x) (g y)`. The return type of `on` is a binary function that takes in two values and returns a third value. E.g.

```haskell
func = (==) `on` (> 0)
func x y = (x > 0) == (y > 0)
```

## `Data.Map`

```haskell
import qualified Data.Map as M

-- creating a map
> keyvals = [("Anu", "779-4290"), ("Mangee", "432-7738"), ("LePeu", "617-3348")]
> phoneBook = M.fromList keyvals
> phoneBook
fromList [("Anu","779-4290"),("LePeu","617-3348"),("Mangee","432-7738")]

> M.empty
fromList []

> M.singleton "three" 3
fromList [("three",3)]

-- serializing a map
> M.toList phoneBook
[("Anu","779-4290"),("LePeu","617-3348"),("Mangee","432-7738")]

-- add a key-value pair to a map
> M.insert 3 100 M.empty  
fromList [(3,100)]

-- check for empty map
> M.null M.empty
True

-- length of a map
> M.length phoneBook
3

-- find elements in the map
> M.lookup "Anu" phoneBook
Just "779-4290"
> M.lookup "Avilay" phoneBook
Nothing
> M.member "Anu" phoneBook
True

-- filter map by value
> M.filter ("77" `L.isInfixOf`) phoneBook
fromList [("Anu","779-4290"),("Mangee","432-7738")]

-- apply some function to all values
> M.map ("206-"++) phoneBook
fromList [("Anu","206-779-4290"),("LePeu","206-617-3348"),("Mangee","206-432-7738")]

-- extract keys and values
> M.keys phoneBook
["Anu","LePeu","Mangee"]
> M.elems phoneBook
["779-4290","617-3348","432-7738"]
```

## `Data.Set`

```haskell
import qualified Data.Set as S

> let fruits = S.fromList ["Apple", "Banana", "Apple", "Mango"]
> fruits
fromList ["Apple","Banana","Mango"]
> let tropicalFruits = S.fromList ["Mango", "Pineapple", "Guava"]
> S.intersection fruits tropicalFruits
fromList ["Mango"]
> S.difference fruits tropicalFruits
fromList ["Apple","Banana"]
> S.union fruits tropicalFruits
fromList ["Apple","Banana","Guava","Mango","Pineapple"]
```

Bunch of other usual functions like - `null`, `size`, `member`, `empty`, `singleton`, `insert`, `delete`, etc.

## `Data.Maybe`
This is for getting the `Maybe` data type.

```haskell
> let a = Just 10
Just 10

let b = Nothing
Nothing
```

## Creating Custom Modules

### Single Module

Lets say I have the following directory structure -

```
.
├── Geometry.hs
└── Main.hs
```

I can import functions defined in `Geometry.hs` inside `Main.hs` like any other module. This is what `Geometry.hs` can look like -

```haskell
module Geometry (sphereVolume, sphereArea) where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * (r ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r ^ 2)
```

The first line indicates the public functions that are exported from this module. This module can contain a bunch of other functions that are “private” and will not be accessible from `Main.hs`.

### Module Heirarchy

I can create a heirarchy of modules much like a Python package.

```
.
├── Geometry
│   ├── Cube.hs
│   └── Sphere.hs
└── Main.hs
```

From inside `Main.hs` I can do `import Geometry.Cube` to get all the functions in `Geometry/Cube.hs`. `Sphere.hs` looks pretty much like before except its module name is now `Geometry.Sphere` -

```haskell
module Geometry.Sphere (sphereVolume, sphereArea) where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * (r ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r ^ 2)
```



