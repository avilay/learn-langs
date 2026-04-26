# Testing

Use the standard Haskell [HUnit framework](https://hackage.haskell.org/package/HUnit). Can be installed in a cabal project with -

```shell
cabal install --lib HUnit
```

There seem to be three main assert functions -

* `assertBool` is like `assert true` in other languages. If the provided boolean variable is `False` the assertion fails and outputs the given error message.

  ```haskell
  -- the assertion will pass for this actual value
  ghci> act = True
  ghci> assertBool "error message here" act
  
  -- the assertion will fail for this actual value
  ghci> act = False
  ghci> assertBool "error message here" act
  *** Exception: HUnitFailure (Just (SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci7", srcLocFile = "<interactive>", srcLocStartLine = 7, srcLocStartCol = 1, srcLocEndLine = 7, srcLocEndCol = 11})) (Reason "error message here")
  ```

  

* `assertString` is weird, if the provided string is an empty string it passes, otherwise it will fail. Not sure why/where I'd use this.

  ```haskell
  ghci> act = ""
  ghci> assertString act
  ghci> act = "some value"
  ghci> assertString act
  *** Exception: HUnitFailure (Just (SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci10", srcLocFile = "<interactive>", srcLocStartLine = 11, srcLocStartCol = 1, srcLocEndLine = 11, srcLocEndCol = 13})) (Reason "some value")
  ```

  

* `assertEqual` is the most common, where the function compares the actual value to the expected value.

  ```haskell
  ghci> exp = 10
  ghci> act = 10
  ghci> assertEqual "error message here" exp act
  ghci> act = 11
  ghci> assertEqual "error message here" exp act
  *** Exception: HUnitFailure (Just (SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci14", srcLocFile = "<interactive>", srcLocStartLine = 16, srcLocStartCol = 1, srcLocEndLine = 16, srcLocEndCol = 12})) (ExpectedButGot (Just "error message here") "10" "11")
  ```

Here is how to write a test case -

```haskell
testMyTestCase :: Test
testMyTestCase = TestCase (assertEqual "message if test fails" expected actual)
```

Here is how to run all the test cases in a single module -

```haskell
-- Test suite
tests :: Test
tests = TestList
    [ 
      TestLabel "empty string" testEmptyString, 
      TestLabel "single character" testSingleCharacter,
      TestLabel "full string" testFullString
    ]

-- Main function to run all tests
runTests :: IO ()
runTests = do
    result <- runTestTT tests
    if failures result > 0 || errors result > 0
        then putStrLn "\nSome tests failed!"
        else putStrLn "\nAll tests passed!"
```





