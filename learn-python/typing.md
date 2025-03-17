# Typing Notes

### Structural Subtyping

#### Protocols

This is a fancy way of saying duck-typing with static types. Here is a classic duck-typing scenario. Lets say we have a function to prepare desserts that can take in any object that a `.bake()` method defined on it. 

```python
def prepare_dessert(sweet) -> None:
    print(f"Preparing {sweet}")
    sweet.bake()
    
    
class Cookie:
    def bake(self) -> None:
        print("Baking cookie")


class Cake:
    def bake(self) -> None:
        print("Baking cake")
        
cookie = Cookie()
cake = Cake()
prepare_dessert(cookie)
prepare_dessert(cake)
>> Preparing <__main__.Cookie object at 0x736184023230>
>> Baking cookie
>> Preparing <__main__.Cake object at 0x736184115160>
>> Baking cake
```

How to annotate the input param `sweet`? I can define a `Protocol` class that defines this one method and no implementation `...`, and make `sweet` an instance of that.

```python
class Bakeable(Protocol):
    def bake(self) -> None: ...


def prepare_dessert(sweet: Bakeable):
    print(f"Preparing {sweet}")
    sweet.bake()
```

This is better than abstract base classes because in here I don't have to change the `Cookie` and `Cake` classes.

#### Type-annotating Complex Callables

`Protcol` classes are also good for defining the type signatures of complex `Callable` types. Lets say I have a function that takes in another function that accepts a variadic number of arguments (with a `*args`). 

```python
# Each vec is a list of floats and the function accepts an arbitrary number of them
def average(pool, *vecs: list[float]) -> int:
  v = pool(*vecs)
  return sum(v) / len(v)
```

How to annotate `pool`? There is no syntax like `Callable[[*list[int]], int]`. Instead, I can create a callable `Protocol` class with the right function signature.

```python
class Pooler(Protocol):
  def __call__(self, *vecs: list[int]) -> list[int]:
    ...
    
def average(pool: Protocol, *vecs: list[int]) -> int:
  v = pool(vecs)
  return sum(v) / len(v)
```

#### Generic Protocols

Les say in our above example, `bake` returns the class itself, i.e, `Cookie.bake` will return `Cookie` and `Cake.bake` will return `Cake`. How do I define the `Protocol` for that?

```python
def prepare_dessert(sweet):
    print(f"Preparing {sweet}")
    return sweet.bake()


class Cookie:
    def bake(self) -> "Cookie":
        print("Baking cookie")
        return self


class Cake:
    def bake(self) -> "Cake":
        print("Baking cake")
        return self


cookie = Cookie()
cake = Cake()
prepare_dessert(cookie)
prepare_dessert(cake)
```

One way is to define `Bakeable` like so -

```python
class Bakeable(Protocol):
    def bake(self) -> "Bakeable": ...
```

But this erases the actual `Cookie` or `Cake` type. Instead I can use generic protocols -

```python
class Bakeable[T](Protocol):
  def bake(self) -> T: ...
```

The pre-3.12 way of doing this is  -

```python
T = TypeVar("T")

class Bakeable(Protocol[T]):
  def bake(self) -> T: ...
```

#### Runtime Checks

The title of this section is a bit of a misnomer, it is not like the protocols can be type-checked at runtime. This is more about using the Protocol with `isinstance ` or `issubclass`.

```python
@runtime_checkable

```





### Typing Alias

This is similar to using `typedef` in C. There are three ways to do this - 

```python
type Vector = list[float]  # type keyword is new in 3.12

Vector = list[float]  # pre-3.12 this is how to do it

from typing import TypeAlias
Vector: TypeAlias = list[float]  # pre-3.12 but making it more explicit
```

#### New Types

Instead of defining an alias, I can define a brand new type without having to create a class for it.

```python
from typing import NewType

ProductId = NewType("ProductId", int)

def display(product: ProductId) -> None:
  print(f"Product is {product}")
  
display(ProductId(1))
# Product is 1
```

However, it does not seem to be a "true" subclass of `int` as can be seen by this -

```python
issubclass(ProductId, int)

>> TypeError: issubclass() arg 1 must be a class
```

As opposed to something like `Amount` defined below, which does happen to be a **true** subclass.

```python
class Amount(int):
    pass
  
issubclass(Amount, int)
>> True
```

Another drawback is that I cannot create subclasses deriving from `ProductId`, however I can create other `NewType`s that are derived from it.

```python
class PremiumProductId(ProductId):
    pass
>> TypeError: Cannot subclass an instance of NewType.
```

```python
PremiumProductId = NewType("PremiumProductId", ProductId)
```



