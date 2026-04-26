# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell learning project using Cabal as the build system. The project is set up as a simple executable application for working through Exercism exercises and learning Haskell fundamentals.

## Build System

This project uses Cabal 3.0+ with GHC 9.6.7. All commands should be run from the project root directory (`/home/avilay/projects/github/learn-langs/learn-haskell/exercises/exercism`).

### Common Commands

- **Build the project**: `cabal build`
- **Run the executable**: `cabal run exercism`
- **Clean build artifacts**: `cabal clean`
- **Start REPL**: `cabal repl` (loads the Main module and dependencies)
- **Build with warnings**: Already enabled via `-Wall` in the common warnings stanza

## Project Structure

- `app/Main.hs`: Entry point for the executable that calls all exercise test suites
- `app/<ExerciseName>.hs`: Individual exercise modules, each containing:
  - The solution implementation
  - Test cases for that exercise
  - A single test suite function that runs all tests for the exercise
- `exercism.cabal`: Project configuration with build settings
- The project uses `Haskell2010` language standard
- Compiler warnings are enabled via `-Wall` for all components

## Exercise Organization

Each Exercism exercise follows this structure:

1. **One file per exercise**: Create `app/<ExerciseName>.hs` for each exercise (e.g., `app/HelloWorld.hs`, `app/LeapYear.hs`)

2. **Module structure**: Each exercise file should contain:
   - Module declaration: `module <ExerciseName> where`
   - The solution function(s)
   - Test cases
   - A test suite function that runs all tests and reports results

3. **Test suite pattern**: Each exercise exports a test suite function (typically named `runTests` or `testSuite`) that:
   - Runs all test cases for that exercise
   - Prints results to stdout
   - Returns `IO ()` so it can be called from `main`

4. **Main.hs integration**: The `app/Main.hs` file should:
   - Import all exercise modules
   - Call each exercise's test suite function from `main`
   - Example:
     ```haskell
     import qualified HelloWorld
     import qualified LeapYear

     main :: IO ()
     main = do
       putStrLn "Running HelloWorld tests..."
       HelloWorld.runTests
       putStrLn "\nRunning LeapYear tests..."
       LeapYear.runTests
     ```

## Development Workflow

When adding a new Exercism exercise:
1. Create `app/<ExerciseName>.hs` with the module, solution, tests, and test suite function
2. Add the module to `other-modules:` in the executable stanza in `exercism.cabal`
3. Import and call the test suite in `app/Main.hs`
4. Run `cabal build` to verify compilation
5. Run `cabal run exercism` to execute all test suites

The executable target is named `exercism` and uses `Main.hs` as the entry point with `main :: IO ()` as the top-level function.
