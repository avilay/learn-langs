# Value Categories

## Lvalues and Rvalues

All expressions evaluate to some value. Value categories is an attempt to categorize the result of expressions so we can make statements like - "The <expr> evaluates to <value category>". There is a [whole list of value categories](https://en.cppreference.com/w/cpp/language/value_category), but here I'll talk about the two most common ones -

**lvalue**: Lvalue expressions are those that evaluate to variables or other identifiable objects that persist beyond the end of the expression. "identifiable" is not very well defined, but a good heuristic to figure out if something is identifiable or not is to see if I can get its address.

**rvalue**: Rvalue expressions are those that evaluate to literals or values returned by functions/ops that are discarded at the end of the expression.

```c++
int x { 5 };
```

`x` is an lvalue and `5` is an rvalue. Our heuristic works because `&x` makes sense and it will compile.

```c++
x = 6;  // OK
7 = x;  // ERROR: Cannot assign to rvalue
```

Here is another example of an rvalue -

```c++
Cookie bake()
{
  Cookie cookie;
  ...
  return cookie;
}

Cookie c = bake();  // The expression bake evaluates to an rvalue.
bake() = someOtherCookie;  // ERROR: Cannot assign to rvalue
```

These are so-called because this is how the compiler decides whether an assignment is legal or not. So how does the following work?

```c++
// x :: int
int y { x };  // Assigning an lvalue to another lvalue?
```

This works because `x` is first implicitly converted to an rvalue, i.e, there is a temporary rvalue with the value of `x` which is then copied to `y`.

