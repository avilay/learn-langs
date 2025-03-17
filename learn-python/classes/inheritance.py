"""
Method Resolution Order
-----------------------
In order to understand constructor behavior, we first need to understand the concept of MRO. Ref
http://python-history.blogspot.com/2010/06/method-resolution-order.html for more details. In a
nutshell, when invoking a method on a object with an object hierarchy, Python has specific rules of
where and in what order it will start looking for this method.

For linear hierarchies, it is as expected, it starts with the most specific, i.e., with the object
on which the call is invoked, and goes all the way upto the mother object.
"""


class Root:
    pass


class LevelOne(Root):
    pass


class LevelTwo(LevelOne):
    pass


LevelTwo.__mro__
# (__main__.LevelTwo, __main__.LevelOne, __main__.Root, object)


"""
For multiple inheritances, it starts as usual with the most specific, and then goes through the
classes from left to right, so in the example below it will first look in LevelOneA and then in
LevelOneB and then in Root.
"""


class Root:
    pass


class LevelOneA(Root):
    pass


class LevelOneB(Root):
    pass


class LevelTwo(LevelOneA, LevelOneB):
    pass


LevelTwo.__mro__
# (__main__.LevelTwo, __main__.LevelOneA, __main__.LevelOneB, __main__.Root, object)

"""
Polymorphism
------------
For most methods this behaves as expected. For any method call, the Python runtime will try to find
the method up the object hierarchy in the MRO starting with the object type that was instantiated.
"""


class Root:
    def froot(self):
        print("Root::froot")

    def groot(self):
        print("Root::groot")
        self.froot()


class LevelOne(Root):
    def froot(self):
        print("LevelOne::froot")


l1 = LevelOne()
l1.groot()  # Will invoke Root::groot -> LevelOne::froot

"""
Below is a clearer demo of polymorphism following the MRO in case of multiple inheritance.
"""


class Root:
    pass


class LevelOneA(Root):
    def only_levelone_a(self):
        print("LevelOneA::only_levelone_a")

    def both_levelones(self):
        print("LevelOneA::both_level_ones")


class LevelOneB(Root):
    def only_levelone_b(self):
        print("LevelOneB::only_levelone_b")

    def both_levelones(self):
        print("LevelOneB::both_levelones")


class LevelTwo(LevelOneA, LevelOneB):
    pass


l2 = LevelTwo()
l2.both_levelones()  # Will look for the method in LevelTwo then LevelOneA and find it there
l2.only_levelone_b()  # Will first look in LevelTwo, then LevelOneA, and then LevelOneB and find it there

"""
But for methods whose names start with double underscores, the behavior is different and unexpected.
It will only look for that method on the object it is currently executing in, i.e., it will not
start from the bottom of the object hierarchy, nor will it go up the hierarchy.

Below is a simple example that it will not go up the hierarchy.
"""


class Root:
    def __froot(self):
        print("Root::__froot")

    def groot(self):
        print("Root::groot")


class LevelOne(Root):
    pass


l1 = LevelOne()
l1.groot()  # Will go up the object hierarchy and find groot in Root.
try:
    l1.__froot()  # Will not go up with object hierarchy to find Root.__froot
except AttributeError as ae:
    print("ERROR: ", ae)


"""
Below is the demo that it will not start from the bottom.
"""


class Root:
    def __froot(self):
        print("Root::__froot")

    def groot(self):
        print("Root::groot")
        self.__froot()


class LevelOne(Root):
    def __froot(self):
        print("LevelOne::__froot")


l1 = LevelOne()
l1.groot()  # Will invoke Root::groot -> Root::__froot

"""And now extending the above example to demo that it will not go up the hierarchy """


class Root:
    def __froot(self):
        print("Root::__froot")


class LevelOne(Root):
    def groot(self):
        print("LevelOne::groot")
        self.__froot()


class LevelTwo(LevelOne):
    def __froot(self):
        print("LevelTwo::__froot")


l2 = LevelTwo()
try:
    l2.groot()  # Will invoke LevelOne::groot and then fail because it cannot find LevelOne::__froot
except AttributeError as ae:
    print(f"ERROR: {ae}")

"""
Using super
-----------
Python's built-in super function will look for the called function in MRO, but only in the object's
parents, not in the object itself.
"""


class Root:
    def froot(self):
        print("Root::froot")


class LevelOne(Root):
    def froot(self):
        print("LevelOne::froot")

    def groot(self):
        print("LevelOne::groot")
        super().froot()  # Will invoke Root.froot inspite of having a LevelOne.froot impl


l1 = LevelOne()
l1.groot()

"""
super works in the multiple inheritance case as expected. In the example below it invokes LevelOneB
when called inside LevelOneA because the MRO is LevelOneA, LevelOneB, and Root.
"""


class Root:
    def froot(self):
        print("Root::froot")


class LevelOneA(Root):
    def froot(self):
        print("LevelOneA::froot")
        super().froot()  # Depending on the MRO, will invoke the next in line's froot


class LevelOneB(Root):
    def froot(self):
        print("LevelOneB::froot")
        super().froot()  # Depending on the MRO, will invoke the next in line's froot


class LevelTwo(LevelOneA, LevelOneB):
    def froot(self):
        print("LevelTwo::froot")
        super().froot()  # LevelOneA.froot > LevelOneB.froot > Root.froot


l2 = LevelTwo()
l2.froot()


""" Example with a different MRO, notice that LevelOneB is before LevelOneA """


class SecondLevel(LevelOneB, LevelOneA):
    def froot(self):
        print("SecondLevel::froot")
        super().froot()  # LevelOneB.froot > LevelOneA.froot > Root.froot


class LevelDo(LevelOneA):
    def froot(self):
        print("LevelDo::froot")
        super().froot()  # LevelOneA.froot > Root.froot


l2 = LevelDo()
l2.froot()

"""
Object construction
-------------------
Python follows a 2 phase object construction, in the first phase it actually constructs the object
and in the second phase it initializes it.

In the first phase - when Python detects that a class is being called, it looks for the
static __new__ method and does go up the object hierarchy even though the name starts with double
underscore. Eventually it will reach object.__new__ which will instantiate a concrete object of
the right type.

In the second phase - Python will call type.__call__ (see callable classes) which in turn will call
the current class's __init__ method with all the args, kwargs that are needed. This also follows
normal rules of polymorphism inspite of having a name that starts with a double underscore. Which
means it will follow the MRO to invoke the first __init__ it finds and calls to super will also
follow the MRO rules.

Below is a demo with a polymorphic __init__
"""


class Root:
    def __init__(self):
        print("Root::__init__")


class LevelOneA(Root):
    def __init__(self):
        print("LevelOneA::__init__")
        super().__init__()  # Depending on the MRO, will invoke the next in line's __init__


class LevelOneB(Root):
    def __init__(self):
        print("LevelOneB::__init__")
        super().__init__()  # Depending on the MRO, will invoke the next in line's __init__


class LevelTwo(LevelOneA, LevelOneB):
    def __init__(self):
        print("LevelTwo::__init__")
        super().__init__()  # LevelOneA.__init__ > LevelOneB.__init__ > Root.__init__


l2 = LevelTwo()

"""
Of course if the calls to super had been missing, it would stop at the first __init__ it finds. Unlike
the constructor semantics in other OO langs, the call is not automatically propagated up the object
hierarchy.
"""


class Root:
    def __init__(self):
        print("Root::__init__")


class LevelOneA(Root):
    def __init__(self):
        print("LevelOneA::__init__")


class LevelOneB(Root):
    def __init__(self):
        print("LevelOneB::__init__")


class LevelTwo(LevelOneA, LevelOneB):
    def __init__(self):
        print("LevelTwo::__init__")
        super().__init__()  # Will stop at LevelOneA.__init__


l2 = LevelTwo()

"""Simple demo of overriding __new__"""


class MyDemo:
    def __new__(cls, *args, **kwargs):
        print("MyDemo.__new__")
        return super().__new__(cls)


demo = MyDemo()

"""
The thing with __new__ is that it can return anything it wants. It does not *have* to return an instance
of the type. Typically, calling super().__new__ at each level will propagate the call all the way to
the mother object.__new__ which knows how to instantiate the object. Another wierdness about __new__
is that it is a static method. It does not have to be decorated as such.
"""


class MyClass:
    def __new__(cls, *args, **kwargs):
        print("MyClass.__new__")
        return 1


obj = MyClass()
print(type(obj))

"""
Demo that object.__new__ instantiates the object and calls __init__
One implementation issue to note here is that if I am overriding both __new__ and __init__ then I
should not call super().__new__(cls, *args, **kwargs) in the base class that is implicitly derived
from object. In this case I only need to call super().__new__(cls). And type.__call__ will do the
rest of the magic. For any other base class that is not implicitly deriving from object, I'll still
need to make the full super().__new__(cls, *args, **kwargs) call.
"""


class MyDemo:
    def __new__(cls, *args, **kwargs):
        print("MyDemo.__new__")
        return super().__new__(cls)


demo = MyDemo()


class MyAwesomeDemo(MyDemo):
    def __new__(cls, *args, **kwargs):
        print("MyAwesomeDemo.__new__")
        return super().__new__(cls, *args, **kwargs)

    def __init__(self, name, duration):
        print(f"MyAwesomeDemo {name} is for {duration} minutes")


bigdemo = MyAwesomeDemo("pitch", 10)

"""
For multilevel inheritance, __new__ is called based on MRO. But just like __init__, it will stop at
the first __new__ impl it finds. For this reason, it is the responsibility of each __new__ impl to
call super().__new__ at the appropriate time. Otherwise the call will not propagate up.
"""
