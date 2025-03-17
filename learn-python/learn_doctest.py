"""Example Module

The example module supplies one function, factorial(). Run this with the following command:
$ python -m doctest -v learn_doctest.py
$ python -m doctest learn_doctest.py

Example:
    Here is a very simple example of correct usage.

    >>> factorial(5)
    120
"""
import math
import sys


def factorial(n):
    """Compute the factorial of the given integer.

    Args:
        n (number): A positive exact integer

    Returns:
        int: The factorial of n.

    Examples:
        >>> [factorial(n) for n in range(6)]
        [1, 1, 2, 6, 24, 120]

        >>> factorial(30)
        265252859812191058636308480000000

        Factorial of negative numbers will raise a ValueError:
        >>> factorial(-1)
        Traceback (most recent call last):
        ...
        ValueError: n must be >= 0!

        Factorials of floats are ok, but the float must be an exact integer:
        >>> factorial(30.1)
        Traceback (most recent call last):
        ...
        ValueError: n must be exact integer!
        >>> factorial(30.0)
        265252859812191058636308480000000

        The input must not be a ridiculously large number:
        >>> factorial(1e100)
        Traceback (most recent call last):
        ...
        OverflowError: n too large!
    """
    if not n >= 0:
        raise ValueError("n must be >= 0!")
    if math.floor(n) != n:
        raise ValueError("n must be exact integer!")
    if n + 1 == n:  # Catch value like 1e300
        raise OverflowError("n too large!")
    result = 1
    factor = 2
    while factor <= n:
        result *= factor
        factor += 1
    return result


if __name__ == "__main__":
    factorial(int(sys.argv[1]))
