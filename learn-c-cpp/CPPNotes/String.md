# String

# C

See string_usage.c

`sprintf` is not used anymore. The alternatives are `snprintf` or the much better `asprintf`. When using `snprintf`, I'll need to use it twice, once to calculate how much space I need to allocate, and second to actually write to the string buffer. Using `asprintf` I don't have to do this, it automatically calculates the space and allocates it. In both cases I have to remember to free the space.

It is possible to use `asprtinf` to concatenate strings like so -

```c
char* name = strdup("Avilay");
asprintf(&name, "My name is %s Parekh", name);
```

However, this is a memory leak. Here is how -

```c
char* name = strdup("Avilay");  // name v0
asprintf(&name /*v1*/, "My name is %s Parekh", name /*v0*/);
free(name /*v1*/);
```

`name v0` never got freed! A kludgy workaround is -

```c
char* name = strdup("Avilay");
char* name_v0 = name;
asprintf(&name, "My name is %s Parekh", name);
free(name_v0);
free(name);
```

A better thing to do is just use `strcat`.

A string created using a `char*` is for all intents and purposes immutable, because changing it after declaration will result in a segfault.

```c
char* s = "Avilay";
s[0] = 'a';  // Will segfault!
```

However, if the pointer is allocated or is an array, it can be edited.

```c
char* s;
asprintf(&s, "Avilay");  // This allocates space for s
s[0] = 'a';  // OK
free(a);  // OK
```

```c
char s[] = "Avilay";
s[0] = 'a';  // OK
```

Alternately I can use `strdup` to duplicate the hard-coded string into an allocated space.

```c
char* s = strdup("Avilay");  // is a null terminated string
s[0] = 'a';  // OK
free(s);  // OK
```

The equivalent to Python's `split` is `strtok` or its more thread-safe replacements - `strtok_r`.  The basic idea is that `strtok` will replace all the delimiters with `\0` and each split will point to the next token in the original string. No new strings are allocated. If I need to free the original string, I need to ensure that I won't need any of the tokens (maybe assign them `NULL`?)

## CPP

See String.cpp.

Just using a string like `"hello"` creates a C-style string, this not `std::string`, but both are interconvertible which is why code like this usually works ` std::string greeting { "Hello" }`. To create proper `std::string` I need to do -

```c++ 
#include <string>

using namespace std::string_literals;

std::string greeting { "Hello"s };  // Note the trailing s
```

Mostly the same interface as a std::vector or std::array 

```c++
// Initialization
std::string greeting { "Hello"s };

// Assignment
greeting = "Namaste"s;

// Read and write
char ch { greeting[3] };
greeting[0] = 'n';
```

Some string specific methods -

```c++
// Concat
std::string name { "Avilay"s };
std::string fullGreeting { greeting + " " + name };
fullGreeting += "!";

// Comparison
if (name == greeting) {...}

// Substring
std::string str { "0123456789" };
size_t idx { 5 };
size_t count { 2 };
str.substring(idx, count);  // "56"
```

Supports finding substrings. It either returns the index where the substring starts or a special value called `std::string::npos`. 

```c++
std::string str { "0123456789" };
auto pos { str.find("56") };
if (pos != std::string::npos) {
    std::cout << str.substr(pos, 2) << std::endl;
}
```

Removal works mostly the same. `erase` works a bit differently. The range is $[i, j]$ inclusive on both ends as opposed to vector, where the iterators are $[i, j)$. 

```c++
std::string str { "0123456789" };
str.erase(1, 3);  // str is now 0456789

str.clear();  // str is ""
```

String/number conversion -

```c++
std::stoi("3.14");  // evaluates to 3
std::stof("3.14");  // evaluates to 3.14
std::stod("3.14");  // evaluates to 3.14
std::to_string(3.14); // evaluates to "3.14"
```

Regex : [C++ Regular Expressions using the Standard Library | A Practical Guide (studyplan.dev)](https://www.studyplan.dev/pro-cpp/regex)

String Views: [String Views in C++ using std::string_view | A Practical Guide (studyplan.dev)](https://www.studyplan.dev/pro-cpp/string-views)

Use string view as input args but not as return values. Return values will result in dangling string views.

