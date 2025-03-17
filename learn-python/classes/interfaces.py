"""
TL;DR: If an ABC does not define an abstract method it does not add much value.

In the example below, even though Cake is an ABC, it can still be instantiated directly. This is
because it does not have any abstract methods defined on it. In this case there is no real benefit
of deriving Cake from ABC unless we want to register some existing classes as subclasses of Cake. We
would do that so that any functions/methods that we write where we are checking if an object is an
instance of Cake, will accept the registered fake Cake subclass. In most cases of non-legacy code
I'll not need to do this.
"""

#%%
from abc import ABC, abstractmethod

class Cake(ABC):
    def bake(self):
        print('Cake::bake')


cake = Cake()
cake.bake()

#%%
"""
When an ABC has an abstract method defined it cannot be instantiated directly. In the example
below Cookie cannot be instantiated directly because it has an abstract method on it. A subclass
that has defined that method can be instantiated. However, it is easy to fool ABC by just matching
the method name and not the method signature. ChocolateCake has a method called eat, but its
signature is not the same as the abstract method eat. But ABC will still allow the class to be
instantiated.
"""
#%%
class Cookie(ABC):
    @abstractmethod
    def eat(self, n):
        print(f'ICookie::eat {n}')


# Cannot be instantiated because it has an abstract method
try:
    cookie = Cookie()
except TypeError as te:
    print(te)


class ChocolateChipCookie(Cookie):
    def bake(self):
        print('ChocolateChip::bake')

    def eat(self):
        print('ChocolateChipCookie::eat')


# Can be instantiated even though its eat method does not have the same signature as the abstract
# eat method
cc = ChocolateChipCookie()
cc.eat()


class SnickerDoodle(Cookie): pass


try:
    sd = SnickerDoodle()  # Cannot instantiate because it has not implemented eat
except TypeError as te:
    print(te)
