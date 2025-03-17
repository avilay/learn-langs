# Smart Pointers

The defining characteristic of a smart pointer is that it manages a dynamically allocated resource by ensuring that this resource is properly cleaned up at the appropriate time, which is usually when the smart pointer goes out of scope. For this reason, smart pointers should never by dynamically allocated themselves. By always allocating smart pointers on the stack (as local variables or class instance variables), we're are guaranteed that the smart pointer will properly go out of scope when the function or the object it is contained within ends, ensuring that the resource owned by the smart pointer is properly deallocated.

> Always allocate smart pointers on the stack, never on dynamically on the heap.

## Unique Pointer

```c++
#include <memory>
std::unique_ptr<Cookie> cookie { new Cookie {} };
```

The unique pointer object will completely own the underlying pointer. The underlying pointer cannot be co-owned by any other entity. This means I cannot reassign unique pointers.

```c++
std::unique_ptr<Cookie> c1 { new Cookie {} };
std::unique_ptr<Cookie> c2 {};  // c2 is holding nullptr
c2 = c1;   // ERROR: Cannot assign unique pointer
```

While I can create a unique pointer object directly using its ctor, I am better off using the builtin in `std::make_unique` factory function to do so -

```c++
auto cookie { std::make_unique<Cookie>(100, "Chocolate Chip") };
// cookie :: std::unique_ptr<Cookie>
```

`unique_ptr` has a cast to `bool` that can tell whether it is managing a resource or is holding a NULL.

```c++
// cookie :: std::unique_ptr<Cookie>
if (cookie) {
  // cookie is holding a valid Cookie*
} else {
  // cookie is holding a NULL
}
```

Both the `operator->` and `operator*` are implemented so I can access the underlying type's methods and attributes, and when the need arises, get a copy of the actual object (not its pointer) as well.

```c++
// cookie :: std::unique_ptr<Cookie>
cookie->getCalories();  // Will call Cookie::getCalories
auto c = *cookie;  // c :: Cookie
```

The last line will copy the underlying `Cookie` object into `c`.

To get the underlying "raw" pointer -

```c++
// cookie :: std::unique_ptr<Cookie>
auto ptr = cookie.get();  // ptr :: Cookie*
```

Unique pointer only supports move semantics, no copy semantics. This means if I want to pass it to a function that accepts the unique pointer by value, I'll have to cast it to an rvalue, which can then be sent over the function. This means that the original unique pointer object now holds NULL.

```c++
void bake(std::unique_ptr<Cookie> cookie) {...}

auto c { std::make_unique<Cookie>(100, "Chocolate Chip") };
bake(c);  // ERROR
```

The above code will not compile because in the last line, it will try to invoke the copy ctor to copy `c` to `cookie`, but `unique_ptr` does not implement copy ctor. 

```c++
bake(std::move(c));  // OK
std::cout << (cookie ? "NOT NULL" : "NULL") << std::endl;  // NULL
```

This will work but then `c` will be left holding NULL. In most cases, I don't want to move the ownership of my underlying pointer to the function. I am much better off doing one of the following -

```c++
void bake(const Cookie* cookie) {...}

auto c { std::make_unique<Cookie>(100, "Chocolate Chip") };
bake(c.get());
```

```c++
void bake(const Cookie& cookie) {...}

auto c { std::make_unique<Cookie>(100, "Chocolate Chip") };
bake(*c);
```

In the second way I'll still have to take the hit of creating a copy by calling `*c`.

It is ok to return a unique pointer from a function -

```c++
std::unique_ptr<Cookie> bakeChocolateChip()
{
  auto c { std::make_unique<Cookie>(100, "Chocolate Chip") };
  return c;
}

auto cookie { c };
```

This will call the move ctor of `unique_ptr` (unless it is elided away) to move `c` into `cookie`.

## Shared Pointer

Multiple shared pointers can share the ownership of the underlying pointer, i.e, they can all hold the same underlying pointer. It keeps a ref count of the number of references to the underlying pointer. As each shared pointer goes out of scope, the ref count is decreased but the underlying pointer is not deleted. Only when the ref count goes to 0 is the underlying pointer deleted.

```c++
#include <memory>
std::shared_ptr<Cookie> cookie { new Cookie {} };
std::shared_ptr<Cookie> copy { cookie };
```

Shared pointer implements copy semantics. When I want to have multiple shared pointers sharing the same resource, I should use the copy ctor (the second line above) to create multiple shared pointers.

This will lead to a segfault -

```c++
Cookie* cookie = new Cookie {};
std::shared_ptr<Cookie> p1 { cookie };
std::shared_ptr<Cookie> p2 { cookie };
```

When this block goes out of scope, both the shared pointers will think that they are the sole owners of `cookie` and will try to destroy it, resulting in segfault at the second line.

This is why it is better to use the `make_shared` factory method to create the first shared pointer. This prevents another shared pointer to directly own the pointer.

```c++
auto c1 { std::make_shared<Cookie>() };
auto c2 { c1 };
```

A unique pointer can be converted to a shared pointer by moving its ownership to the shared pointer. However, a shared pointer cannot be converted to a unique pointer. This is why if I have a function that returns a smart pointer, it is better for it to return a unique pointer rather than a shared pointer.

API -

```c++
// cookie :: shared_ptr<Cookie>
cookie.use_count();  // Gets the ref count
cookie.get();  // Gets the underlying raw pointer
```

## Weak Pointer

Weak pointers can only be created from shared pointers. As such they also share the ref count with the parent shared pointer. However, they don't participate in the ownership of the underlying resource. This means that if there is a weak pointer holding a resource but all the shared pointers have gone out of scope, the resource will be deleted. The weak pointer will be left with a dangling reference.

```c++
// cookie :: shared_ptr<Cookie>
std::weak_ptr<Cookie> weak { cookie };
```

I can use the `.expired()` method to check if the weak pointer is holding a dangling reference. The `.use_count()` method gives the same answer as its parent shared pointer.

The main use case for weak pointer is to break cyclical dependency. If object A has a shared pointer to B and B has a shared pointer to A then even they both go out of scope they will not be deleted. When I have an object holding a reference to another object of the same type, I should use a weak pointer instead of a shared pointer.

Because this does not implement the `operator->` and `operator*`, I cannot actually use the weak pointer to do anything with the underlying pointer. I need to get a shared pointer from it for this.

```c++
// weak :: weak_ptr<Cookie>
auto shared { weak.lock() };  // shared :: shared_ptr<Cookie>
```

