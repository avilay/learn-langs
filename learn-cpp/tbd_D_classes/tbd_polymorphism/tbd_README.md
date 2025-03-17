## Mental Model
Here is a good mental model of the memory layout when I have a pointer or a reference to an object. This is not actually how the memory is laid out, it is just a mental model to help reason about polymorphic behavior.

```
        +=============+
        |    Base     |
        +-------------+
        | baseMethod1 |
        | baseMethod2 |
ptr --> +=============+
```

Now when I call `ptr->baseMethod1()` it will start from the bottom and look for the invoked symbol.

When the object is of type `Dervied` which inherits from `Base` type, this is what it looks like.

```
        +=============+
        |    Base     |
        +-------------+
        | baseMethod1 |
        | baseMethod2 |
        +=============+
        |   Derived   |
        +-------------+
        |  derivedOp  |
ptr --> +=============+
```

Now if I call `ptr->baseMethod1()`, it will start from the bottom and eventually find the implementation of the `Base` class.

### Virtual Methods
When a method is marked as `virtual`, instead of the implementation being inside the object, the function name symbol points to a virtual table which contains the address of the implementation.

```
        +=============+
        |    Base     |
        +-------------+      +----------+
        | baseMethod1-|----> | BaseImpl |
        | baseMethod2 |      +----------+
ptr --> +=============+
```

If `Derived` has not implemented the `virtual baseMethod` then:

```
        +=============+
        |    Base     |
        +-------------+       +----------+
        | baseMethod1-|-----> | BaseImpl |
        | baseMethod2 |       +----------+
        +=============+
        |   Derived   |
        +-------------+
        |  derivedOp  |
ptr --> +=============+
```

Now when I call `ptr->baseMethod1()` it will go upto BaseImpl and run that. On the other hand, if `baseMethod1` has been overridden then:

```
        +=============+
        |    Base     |
        +-------------+
        | baseMethod1-|-------------+
        | baseMethod2 |             |
        +=============+             |
        |   Derived   |             v
        +-------------+       +--------------+
        | baseMethod1-|-----> | DerivdedImpl |
        |  derivedOp  |       +--------------+
ptr --> +=============+
```

Now when I call `ptr->baseMethod1()` it goes to DerivedImpl. Now here is the interesting polymorphic behavior, if I have a `Base *` pointing to a `Dervied` object like so `Base *bptr = new Derived()` then it will look like this:

```
        +=============+
        |    Base     |
        +-------------+
        | baseMethod1-|-------------+
bptr -->| baseMethod2 |             |
        +=============+             |
        |   Derived   |             v
        +-------------+       +--------------+
        | baseMethod1-|-----> | DerivdedImpl |
        |  derivedOp  |       +--------------+
        +=============+
```

And now if I call `bptr->baseMethod()`, it will still call DerivedImpl because of the vtable pointers. Of course with this I cannot call `bptr->derivedOp()` because remember pointers can only see whats above them, so `bptr` cannot go down and find `derivedOp`.

Another slightly subtle behavior is if `baseMethod2()` invokes `baseMethod1()` like so:

```c++
void Base::baseMethod2() {
  // some other code here
  this->baseMethod1();
  // some more code here
}
```

This will also end up calling `DerivedImpl`. This behavior used in the Factory Method design pattern.


### Pure Virtual Methods
If `baseMethod1` is declared as a pure virtual method, then there is no BaseImpl, the vtable points to NULL. This is why it is not possible to instantiate an object of this type. This makes `Base` an abstract base class. Any child class will have to override this method.

```
        +=============+
        |    Base     |
        +-------------+
        | baseMethod1-|----> NULL
        | baseMethod2 |
        +=============+
```
