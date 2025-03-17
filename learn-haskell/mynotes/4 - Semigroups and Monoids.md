# Semigroups and Monoids

## Semigroup

`Semigroup` has a single binary method `<>`. This must obey the mathematical rule of associativity that is not really encforceable in Haskell -
$$
a <> (b <> c) \equiv (a <> b) <> c
$$

```haskell
class Semigroup a where
	(<>) :: a -> a -> a
	
instance Semigroup [a] where
	(<>) = ++
```

Making the list type a semigroup was easy, but it is not always the case. E.g., if I want to implement Semigroup for `Bool`, then I can say that `True <> True = True`, `False <> False = False`, but 

what should `True <> False` return? Because either will satisfy associativity -

Lets say `T <> F = F` then -
$$
T <> (T <> F) = T <> F = F \\
(T <> T) <> F = T <> F = F
$$
Or lets say `T <> F = T` then -
$$
T <> (T <> F) = T <> T = T \\
(T <> T) <> F = T <> F = T
$$
To solve situations like this we can define newtypes that wrap boolean and implement disjunctive as well as concjunctive semigroups. This is how it is done in Haskell standard lib `Data.Monoid` -

```haskell
newtype Any = Any {getAny :: Bool}
newtype All = All {getAll :: Bool}

ghci> import Data.Monoid

ghci> x = All True
ghci> y = All False
ghci> x <> y
All {getAll = False}

ghci> x = Any True
ghci> y = Any False
ghci> x <> y
Any {getAny = True}
```

There are a bunch of newtypes defined just so their semigroups can be implemented unambiguously. E.g., `Sum` and `Product`. But subtraction cannot be a semigroup because it is not associative.

```haskell
newtype Sum a = Sum {getSum :: a}
newtype Product a = Product {getProduct :: a}

ghci> p = Sum 10
ghci> q = Sum 20
ghci> p <> q
Sum {getSum = 30}

ghci> u = Product 10
ghci> v = Product 20
ghci> u <> v
Product {getProduct = 200}
```

## Monoid

`Monoid` has a single property called `mempty` which is supposed to be "neutral" in that it follows the Mathematical laws of identity -
$$
x <> \text{mempty} \equiv x \\
\text{mempty} <> x \equiv x
$$
As can be seen from the definition, anything that implements `Monoid` must also implement `Semigroup`, in other words `Semigroup` is a "superclass" of `Monoid`.

```haskell
class Semigroup a => Monoid a where
	mempty :: a
	
instance Monoid [a] where
	mempty = []
	
instance Num a => Monoid (Sum a) where
	mempty = Sum 0
	
instance Num a => Monoid (Product a) where
	mempty = Product 1
	
instance Monoid Any where
	mempty = Any False
	
instance Monoid All where
	mempty = All True
```

==So far the concepts of monoids and semigroups seem pretty academic at best and pointless at worst.==

