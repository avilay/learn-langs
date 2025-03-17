# Functional

This note is for Haskell like features that have made their way into C++. There is also a `<functional>` header file with a bunch of functional programming like utilities that I also plan to cover here.

## First Class Functions

### Function Pointers

Function pointers make it possible for functions to be first class types in C++. It is a good idea to first create a type alias for a specific function signature.

```c++
using Callback = void (*)(const string&);
```

Now I can use this as a parameter type in another function -

```c++
void fetch(const Url& url, Callback callback)
{
  ...
  callback("Done");  
}
```

And call it like so -

```c++
void alert(const string& msg)
{
  std::cout << msg << std::endl;
}

fetch(url, alert);
```

In C I'd have had to do something like this -

```c
void (*alertPtr)(const string&) = alert;
fetch(url, alertPtr);
```

The reason the C++ syntax works is as follows -

```c++
// Here is how I'd have created a pointer to the alert function
Callback cb { &alert };

// But because of yet another special C++ arcana, I don't have to supply the &, I can just do
Callback cb { alert };
```

Here is a cool example where I can create a workflow -

```c++
using Node = int (*)(int);

int firstNode(int load, Node next)
{
  // Do some work with the load to generate new load
  int nextLoad {...};
  return next(nextLoad);
}

int secondNode(int load)
{
  // Do some work with the load
  int result {...};
  return result;
}

int main()
{
  int load {...};
  int finalResult { firstNode(load, secondNode) };
}
```

### Lambdas

The basic syntax is -

```c++
auto lambda_var { [any, closure, args](any, local, args) {impl;} }
```

I don't have to specify the return type, that is inferred by the compiler. But I can specify it if I want to -

```c++
auto lambda_var { [any, closure, args](any, local, args) -> type {impl;} }
```

Examples -

```c++
auto adder { [](int x, int y) { return x + y; } };

// equivalent to
auto adder { [](int x, int y) -> int { return x + y; } };

// invoke it
int z { adder(10, 20) };
```

Closure values are "captured" in the square brackets. By default they are captured as const by-value.

```c++
int pi { 3.141 };
auto area { [pi](float r) { return pi * r * r; } };
```

The following will give a compile error because x is const inside the lambda -

```c++
int x { 10 };
auto bad { [x]() { x++; } };
```

If I want to capture the closure as a non-const by-value I have to mark the lambda as `mutable`. This will make the variable inside the value as mutable and the lambda will maintain state. But outside the lambda, the variable will still have its old value.

```c++
int x { 10 };
auto bad { [x]() mutable { std::cout << std::format("x={}", x++) << std::endl; } };
std::cout << x << std::endl;
bad();
bad();
std::cout << x << std::endl;
```

This will print -

```
10
x=10
x=11
10
```

> This is just a bad code smell. Don't do this.

To automatically capture all the variables by-value used in my lambda as closures, I can use the following special `[=]` syntax -

```c++
int x { 10 };
int y { 10 };
auto λ { [=](int z) { return x + y + z; } };
int a { λ(1) };  // a = 21
```

In the above code snippet, λ automatically has closure on `x` and `y`. But this is still by value. If I want closure by ref then I can use `[&]` -

```c++
int x { 10 };
int y { 10 };
auto λ { [&](int z) { return x + y + z; } };
int a { λ(1) };  // a = 21
```

I can also have nested lambdas -

```c++
auto nested {
    [](int arg) { return [arg](int x) { return arg + x; }; }
};
```

Here is the equivalent Python -

```python
nested = lambda arg: (lambda x: arg + x)
```

### Callable Objects aka Functors

Making an object of a class callable can be done with the simple trick of overloading the `operator()`. There are some function objects that are simply callables, e.g., the built-in `hash` class.

```c++
struct MyCallable {
  size_t operator()(const std::string& input) {
    return input.length();
  }
};

MyCallable fun {};
auto len { fun("hello") };
```



## Optional

Needs the `#include <optional>` header. Behaves almost like Haskell's `Maybe`.  Quick usage -

```c++
optional<float> inverse(int x)
{
    if (x == 0) {
        return {}; // returns std::nullopt
    } else {
        return 1. / x; // this will automatically push the value inside the optional
    }
}

optional<float> maybe = inverse(2);
if (maybe) {  // checks if optional has a value
  float reciprocal = maybe.value();  // can also use *maybe to extract the value
}

void foo(int x, optional<logger> logger)
{
  if (logger) {
    logger.value().log("inside foo");
  }
  :::
}
```

Syntax -

```c++
optional<int> i { 5 };  // initialize with a value
optional<int> j {};  // initialize with no value
optional<int> k { nullopt };  // initialize with no value

// check if i has a value
if (i.has_value()) {}  
if (i) {}

// get value
int u = *i;  // undefined behavior if i does not have a value
int v = i.value();  // throws exception if i does not have a value
int x = i.value_or(42);  // returns the value if it is there or the default provided
```

## Currying

C++ has surprisingly good support for currying via these APIs -

* `bind()`
* `placeholders`

```c++
Cookie bake(int calories, const std::string& flavor) {...}
auto lowCalorieBake { std::bind(bake, 100, std::placeholder_1) };
Cookie c { lowCalorieBake("Oatmeal Raisin") };
```

## Algorithms

See Algorithms.cpp

All the functions described here are in the `<algorithm>` library.

```c++
// cookies :: vector<Cookie>

// Sorting
sort(
  cookies.begin(),
  cookies.end(),
  [](const auto& x, const auto& y) { return x.calories < y.calories; }
);
// in place sorting, cookies is now changed.

// Count-If
auto count {
  std::count_if(
    cookies.begin(),
    cookies.end(),
    [](const auto& x) { return x.calories < 200; }
  )
};

// Find-If
auto it { std::find_if(
  cookies.begin(),
  cookies.end(),
  [](const auto& x) { return x.calories == 200; }
)};

if (it != cookies.end()) {
  *it;
}

// Iterating
std::for_each(
	cookies.begin(),
  cookies.end(),
  [](const auto& x) { //do something with each element; }
)
```

As seen in the `sort` function, there are a number of functions where I need to pass in a comparator, which is a predicate that takes in two values and checks if one is less than the other. There are callable objects in the standard library that do this.

```c++
#include <vector>
#include <algorithm>
#include <functional>

std::vector<int> nums { 4, 3, 8, 1, 2 };
// sort(nums.begin(), nums.end(), [](int x, int y) { return x < y; });
sort(nums.begin(), nums.end(), std::less {});
```

Other such callable object predicates are - `std::less_equal`, `std::equal_to`, `std::not_equal_to`, `std::greater`, and `std::greater_equal`.

In a similar vein, there are callable objects that implement standard binary arithmetic ops - `std::plus`, `std::minus`, `std::multiplies`, `std::divides`, `std::modulus`, etc.

### Reduce/Accumulate

| Python                                     | C++                                            |
| ------------------------------------------ | ---------------------------------------------- |
| `reduce :: (b -> a -> b) -> [a] -> b -> b` | `accumulate :: [a] -> b -> (b -> a -> b) -> b` |
|                                            | `reduce :: [a] -> a -> (a -> a -> a)`          |



```c++
std::vector nums { 1, -2, 3, -4, 5 };
int result { 
  std::reduce(
    nums.begin(), 
    nums.end(), 
    0, 
    [](int x, int y) {return std::abs(x) + std::abs(y)}
  ) 
};
```

Unlike Python's `reduce`, C++ `reduce` cannot handle multiple types and the user provided operation needs to be commutative and associative. Because this function is thread safe there is no guarantee the order of ops. E.g., if the operator function is `func`, the initial argument is `i`, and the other inputs are `x and y` - then any of the following can happen -

```
a = i; b = x; c = y
func(i, func(x, y))
func(x, func(i, y))
func(func(y, i), x)
...
```

Practically this means that I cannot combine different types, i.e., the initializer should be the same type as the input, and the op function should process two values of the same type.

```c++
std::vector nums { 1, -2, 3 };
float result {
  std::accumulate(
  	nums.begin(),
    nums.end(),
    0.5f,
    [](float acc, int x) -> float { return std::abs(acc) + std::abs(x); }
  )
};
```







