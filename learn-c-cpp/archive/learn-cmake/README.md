## Step 1
There is a single file executable that depends on a single file library. The cmake file builds both the library, and links that to the executable when building the executable. There is also a `TutorialConfig.h.in` that demonstrates the version name trick that I do with my `twine` commands.

```
cd tutorial-step1
mkdir build
cd build
cmake ..
cmake --build .
./Tutorial
```

This will first create a static library in `./build/MathFunctions/libMathFunctions.a` and then link it to an executable that it creats in `./build/Tutorial`. By default `add_library` used in `MathFunctions/CMakeLists.txt` will build a static .a library. But we can ask it to generate shared lib (.lib.so or .dylib). All the other linking stuff happens automatically.

## Step 2
Building on the previous example this makes the library optional, the builder can decide whether or not to build it based on a compiler switch. The executable code chooses which `sqrt` function to use based on the compiler switch.

Because I have the switch set to ON in the top-level CMakeLists.txt, it is defined by default. The following version of `Tutorial` will use my implementation of square root.

```
cd tutorial-step2
mkdir build
cmake -S . -B ./build
```

If I want to turn it off, I can use `ccmake` which is an ncurses version of cmake. Here I can turn the setting OFF and then build again.

```
cd tutorial-step2
ccmake -S . -B ./build
```

Make the changes.

```
cmake --build ./build
```

Now `Tutorial` will use the square root function in `cmath`.

## Step 3
This uses the modern approach. In the old approach, when we decided to use `MathFunctions`, we had to explicitly inlcude the directory so the build system could find the header file `MathFunctions.h`. However, in the modern approach the target can specify what its users need to do in order to use them. In this case we can make `MathFunctions/CMakeList.txt` specify that any component using `MathFunctions` should have the current source directory automatically added to its includes. Once we do this, we no longer need to explicitly add the include dir in the top-level CMakeLists.txt. Everything else remains the same as before.

## Step 4
In this step we add an install target, similar to what `make install` does. We want to install the `MathFunctions` library in `/usr/local/lib` and `MathFunctions.h` in `/usr/local/include`. Additionally, we want to install `Tutorial` executable to `/usr/local/bin` and `TutorialConfig.h` to `/usr/local/include`. 

```
$ cmake --install ./build
-- Install configuration: ""
-- Installing: /usr/local/lib/libMathFunctions.a
-- Installing: /usr/local/include/MathFunctions.h
-- Installing: /usr/local/bin/Tutorial
-- Installing: /usr/local/include/TutorialConfig.h
```

If I want to install root dir to be something other than `/usr/local` I can specify a custom prefix.

## Step 5
In this step we add a test target, similar to `make test`. At at its most basic, the test task will run the executable with the specified args and if the executable returns with non-zero value, the test is deemed to have failed, otherwise passed. I am guessing that I can probably get `pytorch` to work in this framework as well. There are a bunch of other `set_tests_properties` commands where we can put in equivalents of `asserts` but that just seems like a terrible way of writing test cases. So I did not try it out.

```
cd tutorial-step5
mkdir build
cmake -S . -B ./build
cmake --build ./build
cd build
ctest
```

## Step 6
In this step we make a redistributable binary package. Make some changes in the top level CMakeLists.txt to include the `InstallRequiredSystemLibraries` so that any runtime dependencies will be included in the build. Then we include the `CPack` module. That is all that is needed. To generate the default binary and installer do the following:

```
cd tutorial-step6
mkdir build
cmake -S . -B ./build
cmake --build ./build
cd build
cpack
```

This will first generate the installer `Tutorial-1.0-Darwin.sh` and then the binary archive `Tutorial-1.0-Darwin.tar.gz`. The installer is a self-contained CLI installer that will install the binary on the users' computer. Alternately, the tar file can also be just untarred. This will result in 3 directories, `include`, `lib`, and `bin` with all the expected files in them. There is also an option in the `cpack` tool to generate source code for packaging as well, but I am probably not going to use that too often. There are other options to create other types of installables and binary packages.





























