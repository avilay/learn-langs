# Templates

## Function Templates

See FunctionTemplates.cpp

==TODO: what is the difference between `<typename T>` and `<class T>`?==

 Here is the simplest templated function with a single type -

```c++
template <typename T>
T add(T x, T y) {...}
```

Templated functions can have multiple types -

```c++
template <typename T, typename U>
bool areEqual(T x, T u) {...}
```

We can also have templates with non-type template params. In the below function, `N` has to be a constexpr. This is how `std::array` is defined.

```c++
template <typename T, int N>
T power(T val) {...}
```

When invoking a templated function, if the template args can be inferred, we don't need to specify them, just call the function as a normal function, otherwise supply the type args.

```c++
// It is clear I am calling add<int>
int c { add(10, 20) };

// ERROR: Compiler does not know if this is add<int> or add<double>
double z { add(10, 20.2) };  

// Do this instead
double z { add<double>(10, 20.2) };
```

## Class Templates

Example -

```c++
template <class T>
class MyPair {
private:
    T a;
    T b;

public:
    MyPair(T first, T second) : a(first), b(second) {}
    T max() { return a > b ? a : b; }
};
```

