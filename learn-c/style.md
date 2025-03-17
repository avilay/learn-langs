# Style
This note is for the style guide that VSCode will not autolint.

## Webkit

### Spacing

An else if statement should be written as an if statement when the prior if concludes with a return statement.
**Right**:

```c
if (condition) {
    ...
    return someValue;
}
if (condition) {
    ...
}
```

**Wrong**:

```c
if (condition) {
    ...
    return someValue;
} else if (condition) {
    ...
}
```



### Braces

One-line control clauses should not use braces unless comments are included or a single statement spans multiple lines.

**Right:**

```c
if (condition)
    doIt();

if (condition) {
    // Some comment
    doIt();
}

if (condition) {
    myFunction(reallyLongParam1, reallyLongParam2, ...
        reallyLongParam5);
}
```

**Wrong:**

```c
if (condition) {
    doIt();
}

if (condition)
    // Some comment
    doIt();

if (condition)
    myFunction(reallyLongParam1, reallyLongParam2, ...
        reallyLongParam5);
```



Control clauses without a body should use empty braces:

**Right:**

```c
for ( ; current; current = current->next) { }
```

**Wrong:**

```
for ( ; current; current = current->next);
```



### NULL, False, and Zero

Tests for true/false, null/non-null, and zero/non-zero should all be done without equality comparisons.

**Right:**

```c
if (condition)
    doIt();

if (!ptr)
    return;

if (!count)
    return;
```

**Wrong:**

```c
if (condition == true)
    doIt();

if (ptr == NULL)
    return;

if (count == 0)
    return;
```



### Floating Point Literals

Unless required in order to force floating point math, do not append `.0`, `.f` and `.0f` to floating point literals.

**Right:**

```c
const double duration = 60;

void setDiameter(float diameter)
{
    radius = diameter / 2;
}

setDiameter(10);

const int framesPerSecond = 12;
double frameDuration = 1.0 / framesPerSecond;
```

**Wrong:**

```c
const double duration = 60.0;

void setDiameter(float diameter)
{
    radius = diameter / 2.f;
}

setDiameter(10.f);

const int framesPerSecond = 12;
double frameDuration = 1 / framesPerSecond; // integer division
```



### Names

Use CamelCase. Capitalize the first letter, including all letters in an acronym in a struct. Lower case the first letter, including all letters in an acronym, in a variable or a function name.

**Right:**

```c
struct Data;
size_t bufferSize;
```

**Wrong:**

```c
struct data;
size_t buffer_size;
```



Precede boolean values with words like "is" or "did".

**Right:**

```c
bool isValid;
bool didSendData;
```

**Wrong:**

```c
bool valid;
bool sentData;
```



Enum members should use InterCaps with an initial capital letter.



Prefer `const` to `#define`. Prefer inline functions to macros.



Use `#pragma once` instead of `#define` and `#ifdef` for header guards.

**Right:**

```c
// HTMLDocument.h
#pragma once
```

**Wrong:**

```c
// HTMLDocument.h
#ifndef HTMLDocument_h
#define HTMLDocument_h
```



### Types

Omit "int" when using "unsigned" modifier. Do not use "signed" modifier. Use "int" by itself instead.

**Right:**

```c
unsigned b;
int b;
```

**Wrong:**

```c
unsigned int a; // Doesn't omit "int".
signed b; // Uses "signed" instead of "int".
signed int c; // Doesn't omit "signed".
```

