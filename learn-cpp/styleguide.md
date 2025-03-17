# C++ Style Guide

## Google Style

Code files have extension `.cc`. All code files with the exception of main, should have an accompanying header file `.h`. 

All header files must start with `#pragma once` . ==Not strictly Google style.==

`my_header.h`

```c++
#pragma once

namespace packone {
namespace packtwo {

// Note the lack of identation
class MyClass {
 public:
  int CountFooErrors(const std::vector<Foo>& foos) {
    int n = 0;  // Clear meaning given limited scope and context
    for (const auto& foo : foos) {
      ...
      ++n;
    }
    return n;
  }
  
  void DoSomethingImportant() {
    std::string fqdn = ...;  // Well-known abbreviation
  }
  
  ReturnType MyMethod(int, int);
 
 private:
  const int kMaxAllowedConnections = ...;  // Clear meaning within context
  int instance_variable_;
};
}
}
```



`my_impl.cc`

```c++
#include "my_header.h"

ReturnType packone::packtwo::MyClass::MyMethod(int arg_one, int arg_two) {
  // Need to see some code examples to see how the pros do this
}
```

## WebKit Style

Follow the WebKit `clang-format` style.

Some things that are not immediately obvious in the code sample below:

  * Directory names use snake_case.
  * Header files should be in the same directory as its corresponding source file.
  * UTs should be in a seprate directory called `tests` and files should be named `MyClassTest.cpp`.
  * Acronyms should only have the first letter capitalized, e.g., `CnnClassifier.cpp`.
  * Includes should be sorted so that standard headers are first, followed by third party libraries, followed by my internal header files.
  * Identation with 2 spaces.

Contents of **my_cool_ml_app/MyClass.h**

```C++
/*
 * Describe this library. Ok to use old-style C multi-line comments
 * here. But not anywhere else.
 */
#pragma once

#include <iostream>
#include <memory>

#include <boost/lambda/lambda.hpp>
#include <folly/FBString.h>

#include "utils/MyCudaUtils.h"

namespace rocks {
namespace avilay_labs {
///////////////////////////////////

const int kThisIsConstant = 10;

enum class MyEnum {
  UPPER_SNAKE_CASE,
  ONCE_AGAIN
};

class MyClass {
  int privateVar_;

public:
  void publicMethod();
};

}
}
```

Contents of **my_cool_ml_app/CnnClassifier.cpp**

```C++
// Don't use old-style C multi-line comments
// even though they span multiple lines.
int bareFunction() {
  char* flavor;
  MyClass& myObj;
  auto val = theta * x + b;
  if (val == nullptr) {
    fprintf(stderr, "oops!\n");
    std::abort();
  }
}
```
