"""
Any instance method with a double underscore prefix does not have polymorphic behavior. For any
other method on an object, the Python runtime will start from the bottom-most type (the concrete
type instantiated by the caller) and then traverses up the object hierarchy to find the method.
This gives it the polymorphic behavior. But for methods whose name starts with a double underscore
the Python runtime will not traverse up the object heirarchy, it will look for the method on that
concrete object only.
"""

#%%
class Cookie:
    def __crumble(self):
        print('Cookie::__crumble')

    def __save(self):
        print('Cookie::__tear')

    def eat(self):
        print('Cookie::eat')
        self.bake()
        self.__crumble()

    def bake(self):
        print('Cookie::bake')

    @classmethod
    def static_eat(cls):
        print('Cookie::static_eat')
        cls.static_bake()

    @classmethod
    def static_bake(cls):
        print('Cookie::static_bake')


class ChocolateCookie(Cookie):
    def bake(self):
        print('ChocolateCookie::bake')

    @classmethod
    def static_bake(cls):
        print('ChocolateCookie::static_bake')

    def __crumble(self):
        print('ChocolateCookie::__crumble')


cc = ChocolateCookie()

# Even though __save is defined on the Cookie base class, it is not invoked
# because Python runtime will not traverse up the object heirarchy to look for it
# The only place it will look is the object itself.
try:
    cc.__save()
except AttributeError as ae:
    print(ae)

# This will call Cookie.eat -> ChocolateCookie.bake -> Cookie.__crumble
# It will call Cookie.__crumble because it is on the Cookie object when it sees the call to __crumble.
# As mentioned above - it will look for the method on that object only. Note, it does not like
# traversing down the object heirarchy either. bake on the other hand has the expected
# polymorphic behavior.
cc.eat()

# Polymorphic behavior is also seen on static (or classmethods as they are called)
# in Python
ChocolateCookie.static_eat()
