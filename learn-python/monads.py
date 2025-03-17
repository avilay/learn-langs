# Monad: A way to compose naturally non-composable functions.
# In order to be composable with itself, a function should have the same input and output types. More generally,
# in order to compose one function f with another function g, i.e., f * g, the output of g should be of the same type as
# the input of f. But what if this is not the case and we still want to compose f * g? Monads to the rescue. Monads are
# a design pattern that convert f and g such that they have the same input and output types, which makes them composable.
# Lets say we have two functions - both of which take a single string as input and return a list of strings as output.
# In order to compose these, we define a bind function that is a higher order function which takes in a function with signature
# string --> list and returns a function with signature list --> list


def split_on_comma(text):
    return text.split(',')


def split_on_space(text):
    return text.split()


def compose(f1, f2):
    return lambda texts: f1(f2(texts))


def bind(func):
    return lambda texts: [word for text in texts for word in func(text)]


def unit(text):
    return [text]


split = compose(bind(split_on_comma), bind(split_on_space))
toks = split(unit('hi this,is, my guitar'))
print(toks)
