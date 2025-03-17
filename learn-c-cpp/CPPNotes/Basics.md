# Basics

Style guide - [motine/cppstylelineup: a comparison of common C++ style guides (github.com)](https://github.com/motine/cppstylelineup)

I like WebKit

## Initialization

Ref: Init.cpp

All initializations are done with curly-braces nowadays.

```c++
int a;  // Unitialized

int a {};  // initializes to the default value

// Different ways to say a = 26
int a {26};  // decimal
int a {0x1A};  // hex
int a {0b11010};  // binary
int a {032}; // oct
```

```c++
Cookie c;  // Calls the default ctor
Cookie c {};  // Calls the default ctor
Cookie c {180, "Chocolate Chip"};  // calls Cookie(int, string) ctor
Cookie c = {180, "Chocolate Chip"}  // calls Cookie(int, string) ctor
Circle c = 1.0;  // calls Circle(double) ctor
Circle c = {1.0};  // same
Circle c{1.0};  // same

// Same syntax for free memory
Cookie* c = new Cookie {};
Cookie* c = new Cookie {180, "Chocolate Chip"};
```

Implicit narrowing of types will not work anymore -

```c++
int x {1.1};  // ERROR: narrowing from float to int
float y {5};  // OK: y = 5.0

double z {5.5};
float y {z};  // ERROR: narrowing from double to float
float y {5.5};  // OK: See below
```

The last line is ok even though `5.5 :: double` because `5.5` is a `constexpr`, i.e., something whose value and type can be determined at compile time. The compiler is able to tell that this should actually be `5.5f`.

Arrays are also initialized with this -

```c++
int arr[] {};  // arr is a zero length array
int arr[3] {};  // arr = [0, 0, 0] has 3 elements all initialized to the default value
int arr[] {1, 2, 3};  // No need to specify the length here, it is inferred
int arr[3] {1, 2, 3}; // Or I can
int arr[3] {42};  // arr = [42, 0, 0]
int* arr = new int[len] {};  // dynamic length arrays
```

Other ways to initialize arrays -

```c
int* arr = (arr[]){1, 2, 3};
int arr[] = {[2] = 2, 3, [9] = 9};  // arr = [0, 0, 2, 3, 0, 0, 0, 0, 0, 9]
int arr[3] = {};  // arr = {0, 0, 0}
```

If the length of the array is not known at compile time -

```c
int arr[n];
memset(arr, 0, n * sizeof(int));
```

## Printing/Formatting

Before C++20 there was a very funky way of printing out to `cout` by using special keywords like `showbool` and what not. Since C++20 all that nonsense is gone, replaced by a very sane string interpolation library called `format` which is based on Python's format spec. This just returns a string, so I can do with it as I please.

```c++
#include <format>

bool isGood = true;
cout << format("isGood: {}", isGood) << endl;
// isGood: true

double n = 1234.5678901;
cout << format("n = {}, {:.3f}, {:.5e}", n, n, n) << endl;
// n = 1234.5678901, 1234.568, 1.23457e+03

int x = 26;
// Without the leading 0x or 0b or 0 marker
cout << format("x = {:d}, {:b}, {:x}, {:o}", x, x, x, x) << endl;
// x = 26, 11010, 1a, 32

// With the markers
cout << format("x = {:#b}, {:#x}, {:#o}", x, x, x) << endl;
// x = 0b11010, 0x1a, 032

// With caps in the number and marker
cout << format("x = {:#B}, {:#X}", x, x) << endl;
// x = 0B11010, 0X1A
```

See the following format specs for more details -

* [Standard formatter](https://en.cppreference.com/w/cpp/utility/format/spec)
* [Datetime formatter](https://en.cppreference.com/w/cpp/chrono/system_clock/formatter)
* [Other formatter links](https://en.cppreference.com/w/cpp/utility/format/format)

Just like `__repr__` or `__str__` in Python, here I can define my own formatter implementation for any custom type, see [example here](https://en.cppreference.com/w/cpp/utility/format/formatter).

## Auto

Use of `auto` keyword for type inference (good [tutorial here](https://www.learncpp.com/cpp-tutorial/type-deduction-with-pointers-references-and-const/)) -

```c++
auto i { 10 };  // i :: int
```

But `auto` drops consts, constexprs, and references -

```c++
int& foo() {}
auto i { foo() };  // i :: int NOT int&
auto& x { foo() };  // x :: int&

const int k { 10 };
auto j { k };  // j :: int NOT const int
const auto y { k };  // y :: const int

const int& c { 10 };
auto i { c };  // i :: int
const auto j { c };  // j :: const int
auto& k { c };  // !!ATTN!! k :: const int& const is not dropped
const auto& l { c };  // Same as above l :: const int& but the intent is a lot more clear
```

==TODO: Document how auto works with const pointers.== 

## Conditionals

```c++
if (health <= 0) {
  isDead = true;
  dropLoot();
} else if (health <= 50) {
  runAway();
} else if (health <= 100) {
  heal();
} else {
  attack();
}
```

Ternary if operator evaluates to an expression, so this is possible -

```c++
// isDead :: bool
int health {isDead ? 0 : 100};
```

Which is equivalent to -

```c++
int health {100};
if (isDead) {
  health = 0;
}
```

Switch/case -

```c++
// day :: int
switch (day) {
  case 1:
    cout << "Monday";
    break;
  case 2:
    cout << "Tuesday";
    break;
  default:
    cout << "Something else";
}
```

Booleans and integers are sort-of treates as equivalent based on the following rules -

* $0$ cast as a boolean will become `False`.
* Any other number, positive or negative, cast as boolean will become `True`.
* `True` cast as an integer will become $1$.
* `False` cast as an integer will become $0$.

## Functions with Default Params

I can invoke functions with the default values of their types -

```c++
void takeDamage(int damage, int armor) {...}

takeDamage({}, {});
// damage = 0, aromor = 0
```

Functions can have parameters with default values, which makes them optional -

```c++
void takeDamage(int damage, int armor = 100) {...}

takeDamage(10);
// damage=10, armor=100

takeDamage(10, 90);
// damage = 10, aromor = 90
```

However, if I follow the `{}` syntax, then I am passing in the default value of the **type**, not the default value defined for the param.

```c++
takeDamage({}, {});
// damage = 0, armor = 0 (not 100)

takeDamage(10, {});
// damage = 10, aromor = 0 (not 100)
```

An optional parameter cannot be followed by a required parameter -

```c++
void takeDamage(int damage, int armor = 100, bool isMagical) {...}  // ERROR
void takeDamage(int damage, int armor = 100, bool isMagical = true) {...} // OK
```

In general try to avoid the funky `{}` way of passing params. **Explicit is better than implicit.**

## Types

### Classes

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

### Structs

```c++
struct Vector3D {
  double x;
  double y;
  double z;
};
```

 ### Enums

```c++
enum class Color { RED, GREEN, BLUE };

void foo(Color color)
{
  if (color == Color::RED) {...}
}

Color color { Color::RED };
```

There is also a bare enum without the class, but its use is deprecated because it is just a fancy way of labeling integers. In the example below RED is 1, GREEN is 2, and so on.

```c++
enum Color { RED, GREEN, BLUE, ALPHA };
enum Direction { NORTH, SOUTH, EAST, WEST };
Color::RED == Direction::NORTH  // Will be True
```

## Namespaces

```c++
using namespace std;

int main()
{
  cout << "Hello, World!" << endl;
}
```

```c++
int main()
{
  using std::cout;
  cout << "Hello, World!" << endl;
}
```

```c++
int main()
{
  using namespace std;
  cout << "Hello, World!" << endl;
}
```

```c++
enum class Faction { Human, Elf, UnDead };

using enum Faction;
Faction f { Human };

// Without the using I'd have to write -
Faction f { Faction::Human };
```

Type aliases - 

```c++
// make VectPairSI an alias for this crazy type
using VectPairSI = std::vector<std::pair<std::string, int>>;
```

`typedef`s are an old way of defining type aliases and only there for backward compatibility. 

## Exception Handling

Using try/catch -

```c++
#include <exception>

try {
  checked_function();
} catch (std::exception ex) {
  std::cout << ex.what() << std::endl;
}
```

User defined exceptions -

```c++
class MyCustomException : public std::exception {
private:
    std::string msg;

public:
    MyCustomException(const std::string& msg) : msg(msg) {}

    const char* what() const noexcept override {
        return msg.c_str();
    }
};
```

Most of the time I should derive from some other child class of `std::exception`. See [std::exception::exception - cppreference.com](https://en.cppreference.com/w/cpp/error/exception/exception)

 ```c++
 class AppRuntimeError : public std::runtime_error {
 public:
   AppRuntimeError(const std::string &message) : std::runtime_error(message) {}
 };
 
 class ShouldNeverHappenError : public std::logic_error {
 public:
   ShouldNeverHappenError(const std::string &message)
       : std::logic_error(message) {}
 };
 ```

## Attributes

`[[nodiscard]]` Used to mark a function to indicate that its return value should not be discarded. This is usually done for a pure function, because if its output is not used, why was it called in the first place. Will generate a compiler warning if the return value of such a function is discarded.

```c++
[[nodiscard]]
int add(int x, int y) {
  return x + y
}
```

`[[likely]]` and `[[unlikely]]`. Used to mark conditional branch to give the compiler hints on how to optimize the code.

```c++
void startQuest(const Character& player) {
  if (player.isAlive()) [[likely]] {
    ...
  }
}
```

`[[deprecated]]`. Annotate a function (or maybe an entire class?). Will generate a compiler warning when used.

```c++
[[deprecated]]
def startQuest(Character* player) {...}
```

## Misc Syntax

`constexpr` tells the compiler that this expression (can be a function or any other expression) can be determined at compile time and as such can be used anywhere compile time constant values are needed.

```c++
constexpr int product(int x, int y) {...}

const int x { 2 };
const int y { 3 };
const int z = product(x, y);
```

By using `constexpr` I am telling the compiler that this function is never called with runtime values.

Getting the type of a variable or an expression -

```c++
#include <typeinfo>

std::cout << typeid(x).name();
```

Use single quote as thousdands separator.

```c++
int a = 1'000'000
```

## Random

Ref: [Pseudo-random number generation - cppreference.com](https://en.cppreference.com/w/cpp/numeric/random)

The following three things are needed when generating a random number -

* The "device". This is used to generate the random seed. This is a callable object.
* The "engine". This takes in a seed, which is an unsigned int, and can generate random numbers based off of that.
* The distribution. The distribution from which to sample the random number. This is also a callable object that takes the engine as the input.

```c++
std::random_device device {};
auto seed { device() };
std::default_random_engine engine { seed };
std::uniform_int_distribution<int> dist { 1, 10 };
int num { dist(engine) };
```

## Dates & Times

[C++ Basics: Handling Dates, Times, and Durations | A Practical Guide (studyplan.dev)](https://www.studyplan.dev/intro-to-programming/dates-times-duration)

