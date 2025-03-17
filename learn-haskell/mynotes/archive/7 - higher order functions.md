# Higher Order Functions

| Function    | Example                                      | Description                                                  |
| ----------- | -------------------------------------------- | ------------------------------------------------------------ |
| `map`       | `map (+1) [1, 2, 3] = [2, 3, 4]`             | Apply the given function to each element of the given list and returns a new list. |
| `filter`    | `filter even [1..5] = [2, 4]`                | Select only those elements from a list that pass the given predicate function to form a new list. |
| `foldr`     |                                              | Takes in a function, a list, and an initial value, and returns a new function. |
| `.`         | `odd = not . even`                           | Takes in two functions and returns a new function.           |
| `all`       | `all even [2, 4, 6] = True`                  | Check if all elements of a given list pass the given predicate. |
| `any`       | `any even [2, 5, 7] = True`                  | Check if at least one element of the given list passes the given predicate. |
| `takeWhile` | `takeWhile even [2, 4, 6, 7, 8] = [2, 4, 6]` | Keep selecting successive elements from the given list as long as they pass the given predicate. |
| `dropWhile` | `dropWhile even [2, 4, 5, 7, 8] = [5, 7, 8]` | Keep dropping successive elements from the list as long as they pass the given predicate. |

## folding functions

### foldr

This function returns a new function. A very common pattern is to create a function `g` as follows -

```haskell
g :: [a] -> b
g [] = v0
g (x:xs) = f x (g xs)
```

Now applying `g` to a list `[x0, x1, x2]` gives us -

```haskell
g [x0, x1, x2] = f x0 (g [x1, x2])
               = f x0 (f x1 (g[x2]))
               = f x0 (f x1 (f x2 (g [])))
               = f x0 (f x1 (f x2 v0))
               = f x0 (f x1 v1)
               = f x0 v2
               = v3
```

Another way of thinking about the function `g` is that it applies `f` repeatedly on all values of the input list starting with the last element. `f` is a binary function that takes in an element of the input list and a second value and outputs a new value. The new value and the initial second value should be of the same type.
$$
f(x, v) = v' \\
x \in \mathbf X \\
v, v' \in \mathbf V \\
$$

$$
\begin{align}
g(f, v_0, \mathbf x) &= f(x_0, f(x_1, f(x_2, v_0))) \\
&= f(x_0, f(x_1, v_1)) \\
&= f(x_0, v_2) \\
&= v_3 \\
\end{align}
$$

`foldr` is a just a more generic version of `g` wherein in addition to the list `xs` it also takes in the binary function `f` and the initial value `v0` that will be applied to the last element of `xs`.  But because all functions in Haskell can be called as partials, typical usage of `foldr` is to call it with just `f` and `v0` and then apply the returned partial function to a list later on. A simple implementation of `foldr` can be thought of as follows:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v0 [] = v0
foldr f v0 (x:xs) = f x (foldr f v0 xs)
```

### foldl

Similar to `foldr` is the `foldl`. I’ll probably be using this more because there is a Python equivalent in `functools.reduce`. The difference is in the signature of `f`. Here it is $f(v, x) = v'$ and the order of calling `f` inside `foldl`.

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f v0 [] = v0
foldl f v0 xs = f (foldl f v0 (init xs)) (last xs)
```

$$
f(v, x) = v' \\
v, v' \in \mathbf V \\
x \in \mathbf X \\
$$

$$
\begin{align}
g(f, v_0, \mathbf x) &= f(f(f(x_0, v_0), x_1), x_2) \\
&= f(f(v_1, x_1), x_2) \\
&= f(v_2, x_2) \\
&= v_3
\end{align}
$$




 ### Examples

```haskell
-- normal implementation
sum [] = 0
sum (x:xs) = x + sum xs

-- foldr implementation
sum = foldr (+) 0

-- normal implementation
product [] = 1
product (x:xs) = x * product xs

-- foldr implementation
product = foldr (*) 1


-- normal implementation
or [] = False
or (x:xs) = x || or xs

-- foldr implementation
or = foldr (||) False

-- normal implementation
and [] = True
and (x:xs) = x && and xs

-- foldr implementation
and = foldr (&&) True


-- Some more advanced examples

-- normal implementation
length [] = 0
length (x:xs) = 1 + length xs

-- foldr implementation (f is unary function in that it just increments v)
length = foldr (\_ v -> 1 + v) 0


-- normal implementation
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- foldr implementation
reverse = foldr (\x vs -> vs ++ [x]) []


-- normal implementation
(++) [] ys = ys
(++) xs ys = x : xs ++ ys

-- foldr implementation: (++) cannot be directly defined by foldr, but its partial (++ ys) can be
(++ ys) = foldr (:) ys
```

Remember, `f` is always a binary function that is first applied to the last element of `xs` along with `v0`. While the above are some straight forward examples, here are some more clever examples.

There are some more similar functions like `foldl` to process the list from the left, `foldl1` and `foldr1` which use the first element as the accumulator value, `scanl` and `scanr`, etc.

