# PEP 8 Coding Style

## Indentation

- Use 4 spaces per indentation level.

```python
foo = long_function_name(
    var_one, 
	var_two,
    var_three, 
    var_four
)

my_list = [
    1, 2, 3,
    4, 5, 6,
]
```

## Line length

- Limit all lines to a maximum of 79 characters.
- For long lines wrap the expression inside parens.

```python
if width == 0 and height == 0 and (color == 'red' or
                                           emphasis is None):
    raise ValueError("I don't think so -- values are %s, %s" %
                     (width, height))
```

## Blank lines

- Separate top-level function and class definitions with two blank lines.
- Method definitions inside a class are separated by a single blank line.
- Imports should usually be on separate lines. Imports are always put at the top of the file, just after any module comments and docstrings, and before module globals and constants.
- Imports should be grouped in the following order:
  - standard library imports
  - related third party imports
  - local application/library specific imports
- You should put a blank line between each group of imports.
- Put any relevant **all** specification after the imports.
- Use absolute imports like sys.path.

```python
import os
import sys.path
from subprocess import Popen, PIPE

import third_party_lib
import third_party_lib2

import my_lib
import my_lib2


class Cookie:
    def foo:
        pass

    def bar:
        pass


class Cake:
    pass
```

## White space

- Avoid extraneous whitespace in the following situations:

  - Immediately inside parentheses, brackets or braces.
  - Immediately before a comma, semicolon, or colon:
  - Immediately before the open parenthesis that starts the argument list of a function call:
  - Immediately before the open parenthesis that starts an indexing or slicing:
  - More than one space around an assignment (or other) operator to align it with another.

- Always surround these binary operators with a single space on either side: assignment (`=`), augmented assignment (`+=`, `-=` etc.), comparisons (`==`, `<`, `>`, `!=`, `<>`, `<=`, `>=`, `in`, `not` `in`, `is`, `is not`), Booleans (`and`, `or`, `not`).

- Don't use spaces around the `=` sign when used to indicate a keyword argument or a default parameter value.

```python
# Yes:
spam(ham[1], {eggs: 2})

#No
spam( ham[ 1 ], { eggs: 2 } )


# Yes
if x == 4: print x, y; x, y = y, x

# No
if x == 4 : print x , y ; x , y = y , x


#Yes
spam(1)

# No
spam (1)



#Yes
dict['key'] = list[index]

#No
dict ['key'] = list [index]


#Yes:
x = 1
y = 2
long_variable = 3

#No:
x             = 1
y             = 2
long_variable = 3


#Yes:
i = i + 1
submitted += 1
x = x*2 - 1
hypot2 = x*x + y*y
c = (a+b) * (a-b)

#No:
i=i+1
submitted +=1
x = x * 2 - 1
hypot2 = x * x + y * y
c = (a + b) * (a - b)


#Yes:
def complex(real, imag=0.0):
    return magic(r=real, i=imag)
#No:
def complex(real, imag = 0.0):
    return magic(r = real, i = imag)
```

## Naming conventions

- `mypackage`
- `my_module`
- `ClassName`
- `CustomExceptionError`
- `function_name`
- `def instance_method_name(self)`
- `def class_method_name(cls)`
- `_protected_instance_method`
- `_protected_instance_attr`
- `__private_instance_attr`
- `MY_CONST_VAR`
