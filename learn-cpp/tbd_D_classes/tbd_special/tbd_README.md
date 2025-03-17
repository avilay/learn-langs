There are six special members that are implicitly declared on classes under special circumstances.

| Method        | Behavior                                              |
| ------------- | ----------------------------------------------------- |
| Default ctor  | If no other ctors                                     |
| Default dtor  | If no dtor                                            |
| Copy ctor     | If no move ctor or move assign                        |
| Copy assign   | If no move ctor or move assign                        |
| Move ctor     | If no dtor, no copy ctor, and no copy nor move assign |
| Move assign   | If no dtor, no copy ctor, and no copy nor move assign |

In general if a class defines either a copy or move ctor or assignment should either mark the other members as `default` or `delete`.

```c++
class MyClass {
public:
  MyClass(MyClass &copy);
  MyClass& operator=(MyClass& rhs) = default;
  MyClass(MyClass &&anon) = delete;
  MyClass& operator=(MyClass &&anon) = delete;
};
```

The above class has a custom copy constructor, wants to use the default behavior for the copy assignment, and does not want any move semantics.

Here is the full spec of a class that I should always consider.
```c++
class MyClass {
  MyClass();
  ~MyClass();

  MyCLass(MyClass &copy);
  MyClass& operator=(MyClass &rhs);

  MyClass(MyClass &&anon);
  MyClass& operator=(MyClass &&anon);

  // Any other parameterized ctors here
}
```

#### Copy Semantics

```c++
// Lets say obj was already instantiated.
MyClass cp1(obj);  // calls copy ctor
MyClass cp2 = obj;  // calls copy ctor

// Lets say _obj was already instantiated.
_obj = obj;  // calls copy assignment
```

Interestingly enough the following will just invoke the normal ctor -

```c++
MyClass obj = MyClass{};
```

