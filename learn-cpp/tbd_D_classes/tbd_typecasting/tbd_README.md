# Typecasting

## Pointer typecasting

  * Null pointers can be converted to pointers of any type
  * Pointers to any type can be converted to void pointers
  * Pointer upcast: pointers to a derived class can be converted to a pointer of an accessible and unambiguous base class, without modifying its `const` or `volatile` qualification.

### Dynamic cast
This only seems to work with heirarchical pointers. Moreover, when downcasting from base to derived, the base class must have at least one virtual method (what!?!?). Downcasting from base to derived seems to be the only valid use case.

### Static cast
Seems to allow casting from `void *` to concrete class. But does not allow `Apple` to `Orange`. And of course I don't need it for downcasting because dynamic cast will do a better job. No idea what this is used for!!!

### Reinterpret cast
Seems like they put lipstick on explicit casting. What is the use case - no idea!!!

### Const cast
Changes the constness of the pointer, can cast a `const Apple *` to a non const `Apple *`. Of course if we try to write with this pointer it will not work. They why use it? No idea!!!

## Object typecasting
When typecasting types that are not in a heirarchy, the following discussion applies. Pointers on the other hand are a completely different story. Any pointer can typecast to any other pointer!

  1. Type cast operators
  2. Assignment operators
  3. Single argument ctors

### Type cast operators
This is the most common way of typecasting, but should be avoided in favor of `reinterpret_cast`, `dyanmic_cast`, `static_cast`, and `const_cast`.

```c++
Apple apple;
Orange orange;

orange = (Orange)apple;  // this is c-style casting notation
orange = Orange(apple);  // this functional casting notation
orange = apple;  // this will implicitly call the casting operator
Orange orange = apple;  // and so will this
```

In order to enable this sort of type conversion we need to override `operator()` on Apple. The syntax for this is weird as seen in the example.

### Assignment operators
Here is the usage for this:

```c++
Apple apple;
Orange orange;

orange = apple;
```

In order to enable this sort of type conversion we need to override `operator=` on Orange.

### Single argument ctors
The official usage for this is:

```c++
auto apple = Apple();

Orange orange{apple};
Orange orange = apple;
Orange *orange = new Orange(apple);
```

In order to enable this sort of conversion we need to create a ctor which takes `Apple` as input.

One weird thing about this conversion ctor is that if assignment and type cast operator overrides are not present, then those usages will also **implicitly** choose this ctor! In order to avoid this implicit behavior, mark the ctor with the `explicit` keyword. However, this will also prevent `Orange orange = apple` from working because it is also implicitly being converted to `Orange orange(apple)` ctor.