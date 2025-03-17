# Statics & Globals

Depending on how we define functions and variables, they can have internal linkage or external linkage.

**Internal Linkage:** The symbol is visible only in the file it is defined it. It is still available throughout the program lifetime.

**External Linkage:** The symbol is visible to all the files in the program. This too is available throughout the program lifetime.

In the examples below lets assume that we have two files Utils.cpp and Main.cpp which are compiled separately and then linked to produce the final executable. These examples have been implemented in the "linkage-demo".

##### Non-const globals have external linkage by default

I just define, and optinally initialize, the global variable in some file and then forward declare it wherever I want to use it. Forward declarations of variables is done using the `extern` keyword.

```c++
/* Utils.cpp */
int gDefaultCalories { 200 };

/* Main.cpp */
// I still have to forward declare the global using the extern keyword
extern int gDefaultCalories;
:::
int foo()
{
  gDefaultCalories += 100;
}
```

Do not use non-const globals. There is literally no use case that requires its use.

##### Non-const globals with internal linkage

I can force a non-const global to have internal linkage by using the `static` keyword. I can use this from different functions in this file, but not outside of this file.

```c++
/* Utils.cpp */
static int helpValue { 10 };
:::
void helper()
{
  int val = helpValue + 10;
}

/* Main.cpp */
// Cannot use helpValue
```

##### Const globals have internal linkage by default

```c++
/* Utils.cpp */
const string gDefaultFlavor { "Chocolate Chip " };
:::
// Use it within the translation unit
void foo()
{
  cout << gDefaultFlavor;
}

/* Main.cpp */
// Cannot use it from any other file aka translation unit.
```

##### Const globals with external linkage

I can force const globals to have external linkage by using the `extern` keyword when defining them.

```c++
/* Utils.cpp */
const extern int gServingSize { 2 };
:::
// Use it internally as usual
void foo()
{
  cout << gServingSize;
}

/* Main.cpp */
// Can use it anywhere with a forward declaration
extern const int gServingSize;  // Don't initialize it!
:::
int main()
{
  cout << gServingSize;
}
```

##### Functions have external linkage by defualt

```c++
/* Utils.cpp */
void bake(int calories, string flavor)
{
  ...
}

/* Main.cpp */
// forward declare it
void bake(int, string);
:::
// use it
bake(calories, flavor);
```

##### Functions with internal linkage

I can force functions to have internal linkage by using the `static` keyword.

```c++
/* Utils.cpp */
static void helper()
{
  ...
}

/* Main.cpp */
// Cannot use it here.
```

##### Static Local Variables

These are variables that have static lifetime, i.e., they are available from the beginning of the program to its end, they can only be accessed by the scope they are defined in. Here is a trivial example -

```c++
int nextFibonacci() {
    static int first = 0;
    static int second = 1;
    int fib = first + second;
    first = second;
    second = fib;
    return fib;
}
```

This pattern is usually used for a very heavy singleton object.

```c++
int getCalories(const string& flavor)
{
  // Initializing the CalorieFlavor object takes long
  // This is only done the first time the function is called.
  static CalorieFlavor lookup {};
  return lookup.caloriesFor(flavor);
}

int main()
{
  // I cannot access lookup object directly. But I can call the function -
  int calories = lookup("Chocolate Chip");
}
```







