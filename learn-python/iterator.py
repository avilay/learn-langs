"""
Any function that yields a value implicitly returns a generator object. Calling next with the returned
generator object will return a value passed to yield.
"""

#%%
def funciter():
    i = 0
    while i < 3:
        yield i * 10
        i += 1
    print('Done')


genobj = funciter()
print('type(genobj)=', type(genobj))
print('next(genobj)=', next(genobj))
for x in genobj:
    print(x)

for x in funciter():
    print(x)
#%%

"""
Using a generator object in a for loop or any other function like `sum` that accepts a sequence
will first call the builtin `iter` function with the passed in object. If the passed in object is a
generator type then iter will just return the same generator. Then the for loop will implicitly
call next with the object returned by iter until a StopIteration exception is raised.

Lets consider this line:
    for x in funciter():

First `funciter` is called:
    genobj = funciter()

Then the builtin `iter` is called, but in this case it will just return the same genobj
    genobj = iter(genobj)

Then `next` is called and the result put in the variable x
    x = next(genobj)

This is done repeatedly until calling `next(genobj)` raises a StopIteration.

A related concept to an iterator is that of a container class. A container class is nothing but a
class that has implemented the magic __iter__ method.
"""

#%%
class ClassIter:
    def __init__(self, cookie):
        self.cookie = cookie

    def __iter__(self):
        print('Starting the generator')
        i = 0
        while i < self.cookie:
            yield i
            i += 1


container = ClassIter(3)
print('type(container)=', type(container))
genobj = iter(container)
print('type(genobj)=', type(genobj))
for x in container:
    print(x)
#%%

"""
In this case the `container` object is not of type `generator`. When iter is called with container it
will return a generator object. After that for loop will work as usual.
"""
