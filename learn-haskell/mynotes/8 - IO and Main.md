# IO & Main

`IO` is a container, but there is no way of getting anything out of it. We need to use monads or related to use stuff contained inside the `IO` container. The two most common `IO` functions are -

* `putStrLn` to print a line to stdout - `putStrLn "Hello World"`. This function is of type `putStrLn :: String -> IO ()`. The type `()` is a sort of no-op type. This function returns the `IO` container which contains element of this no-op type, i.e., it returns nothing.
* `getLine` to get input from stdin - `getLine :: IO String`



The simplest Haskell program has a module named `Main.hs` which has one function `main` in it. `main :: IO ()`. 

Here is a hello-world main module - 

```haskell
-- name of this file is Main.hs
-- compile this with
-- $ ghc Main.hs -o hello-world
module Main where

main :: IO ()
main = putStrLn "Hello World!"
```

What if I want to print to stdout two times? Just writing `putStrLn` twice is not going to work because `main` is after all just a Haskell function. I can try something like this -

```haskell
main =
	let x = putStrLn "Hello, World!"
	 in putStrLn "Hello, Again!"
```

This will just print "Hello, Again!" because of lazy evaluation. One way to make this work is to use Monads -

```haskell
main = putStrLn "Hello, World!" >> putStrLn "Hello, Again!"
```

This is a good place where we can use the and-then op. In this both the actions return a container, but we don't care about the returned values at all.

This can be rewritten with the do-notation as follows -

```haskell
main = do
	putStrLn "Hello, World!"
	putStrLn "Hello, Again!"
```

A very common pattern in programs is -

1. Get some input from the user or the outside world.
2. Do something with this input.
3. Print out the resulting output or send it to the outside world.

Lets say I want to get a line from the user, reverse it, and print it back out on stdout. I can write this program using Monads like so -

```haskell
printReverse :: String -> IO ()
printReverse line = putStrLn (reverse line)

reverseMe :: IO ()
reverseMe = getLine >>= printReverse
```

Of course `printReverse` is η-reducible to `printReverse = putStrLn . reverse` so I can simplify the definition of `reverseMe` as -

```haskell
reverseMe = getLine >>= putStrLn . reverse
```

But this can be made more readable with the do-notation like so -

```haskell
reverseMe = do
  line <- getLine
  putStrLn . reverse $ line
  
-- or make it even more readable
reverseMe = do
  line <- getLine
  let rev = reverse line
  putStrLn rev
```

Again, in the second line it may seem that I am calling `reverse` with `IO String`, but in reality the do-notation will extract the `String` out of `IO String` and call `reverse` with that.

This kind of pattern of surrounding the pure functions with impure IO related functions is called **Functional Core, Imperative Shell**. 

```haskell
readFile :: FilePath -> IO Text

countWords :: Text -> Map Text Int
findnFrequent :: Map Text Int -> Maybe (Text, Int)
displayResult :: Maybe (Text, Int) -> Text

putStrLn :: Text -> IO ()
```

