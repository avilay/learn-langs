# Casting

See Casting.cpp

In order to convert a value of one type to another, C++ supports 5 types of casting -

* C-style casts
* static casts
* dynamic casts
* const casts
* reinterpret casts

## C-Style Casts

> Avoid using C-style casts.

Depending on the context, C-style casts can actually do all of the other casts. Too much magic, avoid using it. But here is the syntax -

```c++
// x :: int, y :: int
double z { (double)x / y };

double z { double(x) / y};  // Sometimes called function-call syntax
```

### Cast Operator

I can override the `operator Type()` to do C-style casts on completely unrelated types -

```c++
class Orange {...};

class Apple {
  ...
  explicit operator Orange() {...}
}

Apple apple {};
Orange orange { (Orange)apple };
Orange orange { Orange(apple) };
```

### Casting Pointers

* Null pointers can be cast to pointers of any type.
* Pointers of any type can be implicitly cast to void pointers. But going from `void*` to a specific type will require an explicit cast.
* With an explicit cast, pointers of any type can be cast to any type.

```c++
// ptr :: int*
void* v { ptr };

int* i { v };  // ERROR
int* i { (int*) v };

double* j { ptr };  // ERROR
double* j { (double*)ptr };
```

## Static Cast

If I am very sure that the object is castable, I can use the static cast. Unlike C-style casts, it can only do this one thing. 

Main use case is to convert fundamental types - 

* "Broadening" (my term) a type, e.g., int to double.
* Explicit narrowing, e.g., double to int or int to char.

```c++
// x :: int, y :: int
double z { static_cast<double>(x) / y };
```

It also provides compile time type checking.

```c++
int x { static_cast<int>("hello world") };  // ERROR

// x :: const int
int& ref { static_cast<int&>(x) };  // ERROR: Cannot have a mutable ref to an immutable value
```

Can be used for explicit narrowing if I am sure that the narrowing will work -

```c++
double x { 2.5 };
int y { static_cast<int>(x) };
```

## Dynamic Cast

The main use case is in downcasting, where I want to cast a base pointer/reference to the derived pointer/reference. **This only works if the base/derived classes have a v-table, i.e., they have at least one virtual method.**

This works for both pointers and lvalue references. If the cast is not possible, then `dynamic_cast` returns a NULL in case of pointers, and throws a `std::bad_cast` in case of refs.

```c++
// ptr :: const Base*
const auto dptr { dynamic_cast<const Derived*>(ptr) };
if (dptr) {
  // cast was successful.
}

// ref :: const Base&
try {
	const auto dref { dynamic_cast<const Derived&>(ref) };
  // cast was successful
} catch (std::bad_cast err) {
  // cast failed
}
```

## Reinterpret Cast

Do not use it.

## Const Cast

Do not use it.

## Casting lvalue to rvalue

Use `std::move` from the `<utility>` header to cast lvalues to rvalues. This is useful when I have a function that has rvalue params but I only have lvalues in hand.

```c++
#include <utility>

void fun(int&& rref)
{
    std::cout << "rvalue overload: " << rref << std::endl;
}

int i { 10 };
fun(std::move(i));
```

The object whose value has been moved will be in a valid but unspecified state, i.e., its state after the move will be implementation specific. It is best not to make any assumptions about the value after a move. It is ok to give a moved-from object a new value (e.g., using operator=).

