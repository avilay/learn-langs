# Chapter 8 - Type Declarations

## Data types

```haskell
data Shape 
  = Circle Float  -- radius
  | Rectangle Float Float  -- height, width
```

Here I have declared a new type `Shape` with two constructor functions `Circle` and `Rect`. 

To implement the equivalent of Python’s `__repr__` method for the `Shape` type, derive it from `Show` like so -

```haskell
data Shape
	= Circle Float
	| Rectangle Float Float
	deriving (Show)
```

Now if I say `Circle 1` on the REPL it will pretty print the `Shape` object for me. I can verify that `Circle` and `Rectangle` are indeed functions -

```haskell
> :t Circle
Circle :: Float -> Shape

> :t Rect
Rectangle :: Float -> Float -> Shape
```

And I can create different shapes using these functions -

```haskell
unitCircle = Circle 1
unitSquare = Rectangle 1 1
```

Upon checking the type of both `unitCircle` and `unitSquare` I’ll see that they are both `Shape`s.  Ok now that I have my “objects” I need a way to access their member values. Here is where Haskell makes things confusing by using the same syntax as the constructor functions -

```haskell
area :: Shape -> Float
area (Circle r) = 3.14 * r * r
area (Rectangle h w) = h * w
```

Here the stuff in the parens looks like I am creating a new shape, but I am not. This is simply Haskell’s way of binding the member value to the variable `r` so I can use it in the function. If I don’t define the function for all the different types of shapes, I get a linter/compiler(?) warning. The type signature of `area` is `Shape -> Float` and not `Circle -> Float` because it does not make any sense. Just like it does not make any sense for a function to have a type signature `True -> Int`, it must have a signature `Bool -> Int`.

Another example data type -

```haskell
data Point = Point Float, Float deriving (Show) -- x coord, y coord                  
```

Here my constructor function has the same name as my actual data type, both are `Point`. This is customary when there is just a single constructor function for a data type. Now if I redefine my `Shape` data type as follows -

```haskell
data Shape = Circle Point, Float  -- center position, radius
           | Rectangle Point Point  -- upper left position, lower right position
```

And to use these in a function I’ll just have to recursively bind the variables as follows -

```haskell
area :: Shape -> Float
area (Circle _ r) = 3.14 * r * r
area (Rectangle (Point upperLeftX upperLeftY) (Point lowerRightX lowerRightY)) = (upperLeftX - lowerRightX) * (lowerRightY - upperLeftY)
```

### Record Types

To make it easy to keep track of field names we can do 

```haskell
data Shape = Circle {center :: Point, radius :: Float}
           | Rectangle {upperLeft :: Point, lowerRight :: Point}
```

This can help in two ways -

* Make it easy to create shapes
* Make it easy to use shapes in functions

```haskell
area :: Shape -> Float
area (Circle _ r) = 3.14 * r * r
area (Rectangle p1 p2) = (x p1 - x p2) * (y p2 - y p2)

unitCircle = Circle {center={Point x=0 y=0} radius=1.0}
area unitCircle
```

The implementation of area of a circle didn’t change that much. But for area of a rectangle I was able to just bind the point types without recursively going into x and y. Because of the field names, Haskell has automagically provided the functions `x` and `y` that will return those fields for the point object.



## Type Aliasing

```haskell
type String = [Char]

type Scene = [Shape]
```

Similar to C’s typedef.



