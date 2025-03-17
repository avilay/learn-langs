"""
A function can be defined as having required or optional arguments. Further these arguments can be
called as keyword-only or as keyword/positional arguments. It is not possible to declare an argument
as positional-only in versions prior to 3.8. After 3.8, I can use the / symbol in the function arguments
to indicate that all arguments before it are positional only.

Cannot define a function with optional args followed by default args

Cannot call a function with keyword args followed by positional args
"""


# Function with all required keyword/positonal args
def keypos_reqd(arg1, arg2, arg3):
    print(arg1, arg2, arg3)


# Function with all optional keyword/positional args
def keypos_opt(arg1=20, arg2="world", arg3=2.17):
    print(arg1, arg2, arg3)


# Function with some required and some optional keyword/positional args
def keypos_reqd_opt(arg1, arg2="world", arg3=2.17):
    print(arg1, arg2, arg3)


# Function with all required keyword-only args
def keyonly_reqd(*, arg1, arg2, arg3):
    print(arg1, arg2, arg3)


# Function with all optional keyword-only args
def keyonly_opt(*, arg1=20, arg2="world", arg3=2.17):
    print(arg1, arg2, arg3)


# Function with some required and some optional keyword-only args
def keyonly_reqd_opt(*, arg1, arg2="world", arg3=2.17):
    print(arg1, arg2, arg3)


# Function with all required but a mix of keyword and positional args
def key_pos_reqd(arg1, arg2, *, arg3):
    print(arg1, arg2, arg3)


# Function with all optional but mix of keyword and positional args
def key_pos_opt(arg1=20, arg2="world", *, arg3=2.17):
    print(arg1, arg2, arg3)


# Function with some required and some optional but a mix of keyword and positional args
def key_pos_reqd_opt(arg1, arg2="world", *, arg3, arg4=True):
    print(arg1, arg2, arg3, arg4)


def call_with_no_args(func):
    print(f"\n{func.__name__}()")
    try:
        func()
    except (SyntaxError, TypeError) as err:
        print(type(err), err)


def call_with_all_args_positional(func):
    print(f"\n{func.__name__}(10, 'hello', 3.14)")
    try:
        func(10, "hello", 3.14)
    except (SyntaxError, TypeError) as err:
        print(type(err), err)


def call_with_all_args_keywords(func):
    print(f"\n{func.__name__}(arg1=10, arg2='hello', arg3=3.14)")
    try:
        func(arg1=10, arg2="hello", arg3=3.14)
    except (SyntaxError, TypeError) as err:
        print(type(err), err)


def call_with_all_args_mix_of_positional_keywords(func):
    print(f"\n{func.__name__}(10, 'hello', arg3=3.14)")
    try:
        func(10, "hello", arg3=3.14)
    except (SyntaxError, TypeError) as err:
        print(type(err), err)


def call_with_some_args_positional(func):
    print(f"\n{func.__name__}(10)")
    try:
        func(10)
    except (SyntaxError, TypeError) as err:
        print(type(err), err)


def call_with_some_args_keywords(func):
    print(f"\n{func.__name__}(arg1=10)")
    try:
        func(arg1=10)
    except (SyntaxError, TypeError) as err:
        print(type(err), err)


def call_with_some_args_mix_of_positional_keywords(func):
    print(f"\n{func.__name__}(10, arg2='hello')")
    try:
        func(10, arg2="hello")
    except (SyntaxError, TypeError) as err:
        print(type(err), err)


def main():
    funcs = [
        (keypos_reqd, "keypos_reqd(arg1, arg2, arg3)"),
        (keypos_opt, "keypos_opt(arg1=20, arg2='world', arg3=2.17)"),
        (keypos_reqd_opt, "keypos_reqd_opt(arg1, arg2='world', arg3=2.17)"),
        (keyonly_reqd, "keyonly_reqd_opt(*, arg1, arg2='world', arg3=2.17)"),
        (keyonly_opt, "keyonly_opt(*, arg1=20, arg2='world', arg3=2.17)"),
        (keyonly_reqd_opt, "keyonly_reqd_opt(*, arg1, arg2='world', arg3=2.17)"),
        (key_pos_reqd, "key_pos_reqd(arg1, arg2, *, arg3)"),
        (key_pos_opt, "key_pos_opt(arg1=20, arg2='world', *, arg3=2.17)"),
        (key_pos_reqd_opt, "key_pos_reqd_opt(arg1, arg2='world', *, arg3, arg4=True)"),
    ]
    for func, desc in funcs:
        print(f"\n\n**** {desc}")
        call_with_all_args_positional(func)
        call_with_all_args_keywords(func)
        call_with_all_args_mix_of_positional_keywords(func)
        call_with_some_args_positional(func)
        call_with_some_args_keywords(func)
        call_with_some_args_mix_of_positional_keywords(func)
        call_with_no_args(func)
        input("Press ENTER to continue")


if __name__ == "__main__":
    main()
