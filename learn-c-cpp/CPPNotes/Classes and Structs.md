# Classes & Structs

Structs and classes are similar in all aspects except that everything is public by default in struct and private by default in class. Google docs say to use structs for "simpler" data types and classes for everything else. When in doubt use a class. 

## Basic Syntax

(Copied from Basics.md for convenience) 

Basic class -

```c++
class Cookie {
private:
  int calories;
  string flavor;
  
public:
  Cookie(int, string);
  int getCalories();
};

Cookie::Cookie(int calories, string flavor) : calories(calories), flavor(flavor) {}

int Cookie::getCalories()
{
  return calories;
}
```

Basic struct -

```c++
struct Vector3D {
  double x;
  double y;
  double z;
};
```

## Aggregate Initialization and Structured Binding

As long as I have a "simple" struct it follows the following basic syntax for **aggregate initialization** and **structured binding**. ==TODO: What constitues simple?==

```c++
struct Cookie {
    int calories;
    string flavor;
};

// Aggregate Initialization
// Even though there is no explicit ctor, I get this position based ctor for free.
auto cookie = Cookie { 200, "Chocolate Chip" };  

// Structured Binding
// Pull out position based attrs.
auto [cals, flvr] = cookie;
```

### C

```c
typedef struct Cookie {
    int calories;
    char* flavor;
} Cookie;

// <Cookie(calories = 0, flavor = NULL)>
Cookie c1 = {};  
Cookie c2;

// <Cookie(calories = 200, flavor = Chocolate Chip)>
Cookie c3 = {200, "Chocolate Chip"}; 

// <Cookie(calories = 180, flavor = Snicker Doodle)>
Cookie c4 = {.calories = 180, .flavor = "Snicker Doodle"};

// <Cookie(calories = 220, flavor = NULL)>
Cookie c5 = {.calories = 220};
```

Structured binding does not work in C.

#### "Inheritance" in C

Lets say I had a struct called `Vector2D` defined as under -

```c
typedef struct Vector2D {
    float x;
    float y;
} Vector2D;
```

Now I want to extend this struct to a `Vector3D` struct. The simplest way to do this will be to copy/paste the definition of Vector2D into Vector3D as shown -

```c
typedef struct Vector3D {
  struct {
    float x, y;
  };
  float z;
} Vector3D;
```

Now I can use it like -

```c
Vector3D vec = {.x = 1, .y = 2, .z = 3};
```

There is no indication that `.x` and `.y` belong to the nested struct.

However, if there are functions that take in `Vector2D`, I cannot use them with the new object.

```C
float length_xy(Vector2D* vec);
```

The only way to call this would be to create a new Vector2D object `length_xy((Vector2D){.x = vec.x, .y=vec.y});`

The solution is to define the nested struct as a `union` -

```C
typedef struct Vector3D {
  union {
    struct {
      float x, y;
    };
    Vector2D xy;
  };
  float z;
} Vector3D;
```

Now I can do -

```C
Vector3D vec = {.x = 1, .y = 2, .z = 3};
float len = length_xy(vec.xy);
```

And of course I can keep use Vector3D like any old plain struct.

But all of this requires me to copy/paste the original definition of the "parent" struct into the "child" struct which is not good. The solution is to use the `-fms-extensions` compiler flag to unlock a non-standard feature which lets us do this -
```C
typedef struct Vector3D {
  struct Vector2D;
  float z;
} Vector3D;
```

Notice, that the second line is an anonymous object, a regular object would've been declared as `Vector2D xy` or some such. With this I can now initialize and access the child struct as -

```C
Vector3D vec = {.x = 1, .y = 2, .z = 3};
```

But we again lost back-compat with functions that were defined for the old struct. We can reuse the union trick again with this new syntax -

```C
typedef struct Vector3D {
  union {
    struct Vector2D;
    Vector2D xy;
  };
  float z;
} Vector3D;
```

## Operator Overloading

Basic syntax for operator overloading -

```c++
struct Vector3D {
    float x;
    float y;
    float z;

    Vector3D operator-(Vector3D other)
    {
        return Vector3D { x - other.x, y - other.y, z - other.z };
    }
};

Vector3D operator+(Vector3D a, Vector3D b)
{
    return Vector3D { a.x + b.x, a.y + b.y, a.z + b.z };
}
```

## Constructors

All classes and structs have a default constructor that initalizes all the member variables to their type defaults. However, if I implement my own constructor, even if it is not the default constructor, I lose the system provided default constructor. To get it back I can use the `= default` as shown below:

```c++
class Circle {
private:
  double radius;
  
public:
  Circle() = default;
  Circle(double circumference);
};
```

To write non-default parameterized constructors follow the below syntax called member initialization -

```c++
class Circle {
private:
	double radius;
  
public:
	Circle(double radius) : radius(radius) {}  
};

class Cylinder {
private:
  double height;
  Circle base;
  
public:
  Cylinder(double height, double radius) : height(height), base(radius) {}
};
```

Notice the weird syntax `base(radius)`, this will call the appropriate constructor for the type of `base`.

### Details: Why not to use the "old" way of defining constructors

If I do it the old way or the way it is done in other OO languages then the member variables will first be assigned their type default even before the constructor starts executing. Once inside the constructor code, the right values that I want will be set. e.g.,

```c++
class Circle {
private:
  double radius;
  
public:
  Circle(double circumference) {
    radius = circumference / (2*pi);
  }
}
```

Here before the code reaches the part where the radius is calculated from the circumference, radius has already been initialized to its typef default. This is not such a big deal for this class, but consider the next class which has Circle as one of its members. The default initializer for a user defined type (class or struct) is its default constructor, which in the case of Circle is missing.

```c++
class Cylinder {
private:
  double length;
  Circle base;
  
public:
  Cylinder(double volume) {
    // ERROR: The compiler will try to find the default constructor for Circle and fail.
  }
}
```

Of course I can provide a default constructor to deal with this error, but in general a good idea to avoid this kind of constructors. 

### Constructor Delegation

While obvious, it bears mentioning, it does not make sense for one constructor to call another consructor in its body.

```c++
class Cookie {
public:
  Cookie(int calories, string flavor);
  Cookie(string flavor)
  {
    Cookie(0, flavor);  // DOES NOT MAKE SENSE: I just created a temp cookie object.
  }
};
```

On the other hand, I can call other constructors in the initializer.

```c++
class Cookie {
public:
  Cookie(int calories, string flavor);
  Cookie(string flavor) : Cookie(0, flavor) {}
};
```

However, I cannot do both member initialization and call other constructors in the same initializer.

```c++
class Cookie {
public:
  Cookie(string flavor);
  Cookie(int calories, string flavor) : calories(calories), Cookie(flavor) {}  // ERROR
};
```

This is why constructors with less parameters delegate to constructors with more parameters.

### Copy Constructor and Copy Assignment

see CopyAndMove.cpp and CopyAndMoveExample.cpp

The system provides both copy constructor and copy assignment operator by default. They both do a shallow copy which for most purposes is ok unless I am using pointers in my attributes, in which case I should override them. A lot of times the compiler will optimize the copy constructor call away, this is called [**copy elision**](https://www.learncpp.com/cpp-tutorial/class-initialization-and-copy-elision/). 

> There is a `clang++` flag called `-fno-elide-constructors` which will not optimize the move or the copy ctors away. Use this flag in CopyAndMove.cpp.

If I want my class to not support copy semantics, I can delete the system provided copy ctors -

```c++
class Cookie {
public:
  :::
  Cookie(const Cookie&) = delete;
};
```

Here are the scenarios where copy constructor is invoked -

```c++
// c1 :: Cookie
// bake :: Cookie -> void
Cookie c2 { c1 };
Cookie c3 = c1;
bake(c1);
```

Here is trivial implementation that is provided by default anyway -

```c++
class Cookie {
  int calories;
  std::string flavor;
  
public:
  Cookie(const Cookie& copy) : calories(copy.calories), flavor(copy.flavor) {}
};
```

Here is the only use of copy assignment I have found so far -

```c++
// c1 :: Cookie and c2 :: Cookie
c1 = c2;  // Why would I want to do this?
```

Implementing the copy assignment when my class is managing resources or has a pointer to memory, is trickier than it first seems, one typically needs to use the [copy and swap idiom](https://stackoverflow.com/questions/3279543/what-is-the-copy-and-swap-idiom) for this. Copy assignment can have the following possible signatures (left out a couple of `volatile` param types)-

```c++
class Cookie {
private:
  int calories;
  string flavor;
  
public:
  Cookie& operator=(Cookie other);  
  Cookie& operator=(Cookie& other); 
  Cookie& operator=(const Cookie& other) // Use this so it won't clash with Move assignment
  {
    // Here is a trivial implementation
    if (this == &other) {
      return *this;
    }
    calories = other.calories;
    flavor = other.flavor;
    return *this;
  }
};
```

### Move Constructor and Move Assignment

The system provides both move constructor and move assignment by default. As with the corresponding copy methods, these also just do a shallow copy. Again, this ok for most purposes unless my class is holding an underlying resource or a pointer, in which case I need to provide my own implementation.

When a function is returning an object by value, I'd think that it will invoke the copy ctor to copy the object from the function stack onto the caller's stack. And if no move ctor is present, this is what will happen. But C++ has a special rule for this scenario, where it will call the move ctor instead of copy.

Here are the scenarios where move ctor is called -

```c++
// bakeChocolateChip :: void -> Cookie
Cookie c1 { bakeChocolateChip() };
Cookie c2 = bakeChocolateChip();
```

And here is the scenario where move assignment is called -

```c++
// bakeChocolateChip :: void -> Cookie
// c :: Cookie
c = bakeChocolateChip();
```

Both of these methods need the `noexcept` tag. Here are the trivial implementations -

```c++
class Cookie {
  int calories;
  std::string flavor;
  
public:  
  :::
  Cookie(Cookie&& move) noexcept
        : calories(move.calories)
        , flavor(move.flavor)
    {}
  
  Cookie& operator=(Cookie&& other) noexcept
    {
        if (this == &other) {
            return *this;
        }

        calories = other.calories;
        flavor = other.flavor;
        return *this;
    }
};
```

## Destructors

TODO

## Rule of Five

If a class requires one of user defined copy constructor, copy assignment, move constructor, move assignment, or destructor, it probably requires all five.

## Callable Classes

See Functional.
