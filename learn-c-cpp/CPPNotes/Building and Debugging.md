# Building & Debugging

Ref: `make_demo` directory.

## Basic

In C++, the compilation process is broken down into two processes - first our files are ***compiled***, and then they are ***linked***. 

The compiler or rather the pre-processor reads each of our source files from top to bottom, includes all the `#include` files and pre-processes all the pre-processor directives in the resulting file. This generates an intermediate output, sometimes referred to as a **translation unit**. 

The linker then takes these translation units and links them together to create a single cohesive package. A forward declaration of a function is a promise to the compiler that the thing we're trying to use is going to be defined somewhere else. We’re telling the compiler that the linker will take care of it.

See [Driving Compilers (fabiensanglard.net)](https://fabiensanglard.net/dc/index.php) for a good explanation of this process.

### Building a Shared Library

Lets say I want to build a library with the following directory structure:

```
aptg-stuff/
	stuff.h
	stuff.cc
```

I can build a shared library with the following command:

```shell
cd aptg-stuff
clang++ -std=c++17 -dynamiclib -o libAptgStuff.dylib stuff.cc

# or

g++ -shared -o libAptgStuff.dylib stuff.cc
```

This will create the `libAptgStuff.dylib` file in `aptg-stuff`.  Usually when building shared libraries it is useful to have the linker generate "position independent code" which allows the compiled code to be loaded at any memory location rather than a fixed one. For some bizarre reason this does not seem to be the default setting in `gcc`, not sure about `clang`. So when building shared libraries it is best to use -

```shell
gcc -shared -fPIC -o libAptgStuff.so stuff.c
```

I tried to find if there is a way to tell if a .so was built with the `-fPIC` option or not. There are ways to do this on Ubuntu/Debian, but I did not find a way to do this on Arch.

Typically I want to "install" this library in some well known location.

```shell
sudo cp libAptgStuff.dylib /usr/local/lib
sudo cp stuff.h /usr/local/include
```

### Building an Executable

Now lets say I have an executable that wants to use `AptgStuff`. The executable project directory structure looks like this -

```
aptg-app/
	main.cc
```

Inside main.cpp I'll typically have a line `#include <stuff.h>` and then use the functions exported in this header file in my code. To build the executable, I have to specify that I want to link with the shared library `libAptgStuff.dylib` and tell the linker where to find the binary.

```shell
$ cd aptg-app
$ clang++ -std=c++17 -o aptgapp -L/usr/local/lib -lAptgStuff -I/usr/local/include main.cc
```

In the above command the `-L` switch will augment the `LD_LIBRARY_PATH` with the paths mentioned here. I can specify multiple paths with a ":" separator, just like any other path. Of course I could've simply added this path to my `LD_LIBRARY_PATH` but that is not recommended. Next the `-l<name>` switch says to find a library named `lib<name>` in the library paths. Notice that I don't have to give the full `lib<name>.dylib` filename here. Finally the `-I` path tells the compiler where to find the header file `stuff.h`. I can have as many `-I` switches as I want.

For C I am going to start using `gcc` because it has full support for C17. Clang has only partial support for this. Mostly `clang` options and flags are fully compatible with `gcc`. I can specify specific C versions using the same `-std=c23` or `-std=c2y` or `-std=c11`.  GCC provides some extensions that are not part of the C lang specs. These are specified as `-std=gnu11`.  By default GCC supports `-std=gnu17`. 

### Runtime Linking

Unlike static libs, shared libs are linked to the program at runtime, which means that I have to locate the library twice - once when building it on my own machine, and second when my user is running my program on their machine. When an executable is built, it usually carries this information. To see the exact locations where the executable expects the linked libs, I can use `otool` on Mac OS or `ldd` on Linux. Running `otool` on `aptgapp` gives -

```shell
$ otool -L aptgapp 
aptgapp:
        libAptgStuff.dylib (compatibility version 0.0.0, current version 0.0.0)
        /usr/lib/libc++.1.dylib (compatibility version 1.0.0, current version 904.4.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1292.60.1)
```

The above output says that `aptgapp` expects `libAptgStuff.dylib` in the same directory as itself. It also uses two other libraries located at `/usr/lib/libc++.1.dylib` and `/usr/lib/libSystem.B.dylib`. 

For this same info I can run `ldd` without any options -

```shell
ldd /path/to/aptgapp
```

If the library is in one of the "usual locations", the system will find it. However, for libraries in non-standard paths, I need some way to tell the executable where it can find it on the host machine, i.e., pass the information of `-L` flag to the executable somehow. This is done via -

```shell
g++ -o aptgapp -L/usr/local/lib -lAptgStuff -Wl,-R/usr/local/lib -I/usr/local/include main.cc
clang++ -std=c++17 -o aptgapp -L/usr/local/lib/aptg -lAptgStuff -Wl,-rpath,/usr/local/lib -I/usr/local/include main.cc
```

The difference from before is the `-Wl` flag with the CSV. This is simply telling the compiler to pass this information down to the linker which then bakes the `-R` or `-rpath` value into the executable.

As a user, if the paths on my host machine are still messed up, i.e., the path shown by `otool` or `ldd` is incorrect, I can "path" the executable with the correct paths using `patch-elf` (Linux) or `install_name_tool` (Mac OS). 

```shell
$ install_name_tool -change libAptgStuff.dylib /usr/local/lib/libAptgStuff.dylib aptgapp 
$ otool -L aptgapp 
aptgapp:
        /usr/local/lib/libAptgStuff.dylib (compatibility version 0.0.0, current version 0.0.0)
        /usr/lib/libc++.1.dylib (compatibility version 1.0.0, current version 904.4.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1292.60.1)
```

For libs that are not in the usual places and are not annotated via the `-Wl,R...` option, this path can be set in the `LD_LIBRARY_PATH` envvar. 

If I build my library using CMake, it adds an `@rpath` in the library such that when I build my executable, the path of the library shows up as `@rpath/libCMakeAptgStuff.dylib`. This seems like a MacOS specific thing. Some blogpost said that it can be set with the environment variable `LD_RUNPATH_SEARCH_PATH` but when I tried it, it did not work. 

### Compiler Flags

#### Warning

```
-Wall -Weffc++ -Wextra -Wconversion -Wsign-conversion -Werror
```

The last one `-Werror` will treat warnings as errors.

To ignore specific warnings see [Errors and Warnings of Clang-enhanced C++ Compilers - RAD Studio (embarcadero.com)](https://docwiki.embarcadero.com/RADStudio/Athens/en/Errors_and_Warnings_of_Clang-enhanced_C%2B%2B_Compilers#:~:text=Controlling Warning Messages,-The option Enable&text=To disable that warning%2C replace,warnings except for unused variables.)

#### Optimize

```
-O3
```

This is the highest level of optimization. It is totally ok to use this with `-g` setting. For the lowest, or no compiler setting use `O0`, which is the default anyway.

## Makefile

All Unix-based systems come bundled with a `make` tool that looks for a file named `Makefile` in the current directory and processes it. This section how to author Makefiles.

Lets say I have a simple directory structure -

```
.
├── Makefile
├── app
│   └── main.cpp
└── utils
    ├── Point.cpp
    ├── Point.h
    ├── Triangle.cpp
    └── Triangle.h
```

In order to build my app I'd have given the following CLI command -

```shell
clang++ -std=c++20
        -g
        -Wall
        -I.
        app/main.cpp utils/Point.cpp utils/Triangle.cpp
        -o bin/opengldemo
        -lglfw.3 -lGLEW.2.1
        -framework OpenGL
        -frameework Cocoa
        -framework IOKit        
```

Here is each line explained -

* `-std=c++20` tells the compiler to compile using C++20.
* `-g` generates the source-level debug information as part of the executable. There is no separate pdb file.
* `-Wall` tells the compiler to treat all warning as errors.
* `-I.` to tell the compiler that whenever needs to include header files, it should look into these directories in addition to wherever it is looking.
* List of cpp files that need to be compiled and linked.
* `-o` name and location where the output file should be placed.
* Link with `libglfw.3.dylib` and `libGLEW.2.1.dylib`
* Use the specified frameworks. ==TODO: What is the difference between a framework and a library?==

Makefiles have the following structure -

```
<target>: <list> <of> <deps>
\t<cmd> <to> <run>
```

The command should be seperated by a [tab] not spaces. make is very finicky about this.

So we can make a drop-dead simple Makefile with these lines -

```makefile
opengldemo: app/main.cpp utils/Point.cpp utils/Triangle.cpp
	clang++ -std=c++20 -g -Wall -I. app/main.cpp utils/Point.cpp utils/Triangle.cpp -o bin/opengldemo -lglfw.3 -lGLEW.2.1 -framework OpenGL -framework Cocoa -framework IOKit
```

The first line is saying that the target `opengldemo` depends on the three files. If any of the three files change, the target will have to be rebuilt.

To run this command I'll say -

```shell
make opengldemo
```

The first target defined in the makefile is the default target, so I don't even have to give the target name, I can just say -

```shell
make
```

The target name can be anything, it is just for clarity and convention that I have named it the same as the output executable.

The `clang++` command is doing a lot of heavy lifting -

* It first compiles all the source files into object files.
* It links all the object files together.

I can separate these steps out in the makefile -

```makefile
opengldemo: main.o point.o triangle.o
	clang++ -std=c++20 -g -Wall -I. -o bin/opengldemo -lglfw.3 -lGLEW.2.1 -framework OpenGL -framework Cocoa -framework IOKit bin/main.o bin/Point.o bin/Triangle.o
	
main.o: app/main.cpp utils/Point.h utils/Triangle.h
	clang++ -std=c++20 -c -g -Wall -I. -o bin/main.o app/main.cpp
	
point.o: utils/Point.cpp utils/Point.h
	clang++ -std=c++20 -c -g -Wall -I. -o bin/point.o utils/Point.cpp
	
triangle.o: utils/Triangle.cpp utils/Point.h
	clang++ -std=c++20 -c -g -Wa ll -I. -o bin/triangle.o utils/Triangle.cpp
```

The `-c` option tells `clang++` to only compile, not link. I have included the header files as dependencies even though they don't appear in the subsequent command so that it will trigger a rebuild if any of the header file changes.

Here the first line says that `opengldemo` target depends on three other entities. It cannot find any files with these names in the current directory structure, but rather finds these defined as targets. It goes to the first dependent target `main.o` and sees that it depends on `app/main.cpp` and there is indeed a file by that name. If that has changed, then this target needs to be rebuilt. What follows is the command needed to build this target. Similarly for the other two targets. After all the dependent targets have been built, make comes back to the original target `opengldemo` and then runs the command specified there.

However, there are some more improvements that can be made using variables -

```makefile
CC=clang++
CFLAGS=-std=c++20 -Wall -I.
DBGFLAGS=-g
HEADERS=utils/Point.h utils/Triangle.h
LIBS=-lglfw.3 -lGLEW.2.1
FWORKS=-framework OpenGL -framework Cocoa -framework IOKit

opengldemo: main.o point.o triangle.o
	$(CC) $(DBGFLAGS) $(CFLAGS) -o bin/opengldemo $(LIBS) $(FWORKS) obj/main.o obj/Point.o obj/Triangle.o
	
main.o: app/main.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o obj/main.o app/main.cpp
	
point.o: utils/Point.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o obj/point.o utils/Point.cpp
	
triangle.o: utils/Triangle.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o obj/triangle.o utils/Triangle.cpp	
```

I could've skipped putting frameworks and libs in their own variables because they are only ever used once, but I did it anyway for aesthetics. Also, now `point.o` unecessarily depends on `utils/Triangle.h`, but that is ok to keep things simple.

All the object file targets follow the same build pattern. This too can be made more succint and reusable by using the following four special symbols that make recognizes -

* `%.<ext>` - This shows up as a target name or a dependency, e.g, `%.o: <some> <deps>`. Now whenever make is looking for a target named `main.o` or `point.o`, it will substitute `%` with `main` or `point` and run the build specified by the `%.o` target.
*  `$@` - This shows up in the build command. It refers to the target name, or more specifically everything to the left of `:`.
* `$^` - This shows up in the build command. It refers to all the dependencies, or more specifically everything to the right of `:`.
* `$<` - This shows up in the build command. It refers to the first dependency.

First obvious thing to do is -

```makefile
%.o: app/%.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o bin/%.o app/%.cpp
```

But I cannot use `%` in the build command. I can replace `app/*.cpp` with `$<`. But what to do about the output file? I can use a clever trick here where I can name the targets as `obj/main.o` and `obj/point.o` and so on. Since these are just labels, it does not matter what I call them.

```makefile
obj/main.o: app/main.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o obj/main.o app/main.cpp
```

can be rewritten as -

```makefile
obj/%.o: app/%.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o $@ $<
```

Here are the substitutions -

* `%` $\leftarrow$ `main`
* `$@` $\leftarrow$ `obj/main.o`
* `$<` $\leftarrow$ `app/main.cpp`

The only other change I have to make is to replace the depedencies of the `opengldemo` target with `obj/main.o` and so on. Here is what the makefile looks like so far -

```makefile
CC=clang++
CFLAGS=-std=c++20 -Wall -I.
DBGFLAGS=-g
HEADERS=utils/Point.h utils/Triangle.h
LIBS=-lglfw.3 -lGLEW.2.1
FWORKS=-framework OpenGL -framework Cocoa -framework IOKit

opengldemo: obj/main.o obj/point.o obj/triangle.o
	$(CC) $(DBGFLAGS) $(CFLAGS) bin/opengldemo $(LIBS) $(FWORKS) bin/main.o bin/Point.o bin/Triangle.o
	
obj/%.o: app/%.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o $@ $<
```

I can use `$^` to make the `opengldemo` target better -

```makefile
opengldemo: obj/main.o obj/point.o obj/triangle.o
	$(CC) $(DBGFLAGS) $(CFLAGS) bin/opengldemo $(LIBS) $(FWORKS) $^
```

And while I am at it, might as well collect all the object files into their own variable -

```makefile
OBJ=obj/main.o obj/point.o obj/triangle.o

opengldemo: $(OBJ)
	$(CC) $(DBGFLAGS) $(CFLAGS) bin/opengldemo $(LIBS) $(FWORKS) $^
```

Another useful variable will be the output object directory -

```makefile
ODIR=obj
OBJ=$(ODIR)/main.o $(ODIR)/point.o $(ODIR)/traingle.o
```

As can be seen a variable can be used to define another variable.

After a few more directory name variables, here is the final makefile -

```makefile
CC=clang++
CFLAGS=-std=c++20 -Wall -I.
DBGFLAGS=-g
HEADERS=utils/Point.h utils/Triangle.h
LIBS=-lglfw.3 -lGLEW.2.1
FWORKS=-framework OpenGL -framework Cocoa -framework IOKit
BUILDDIR=.
ODIR=$(BUILDDIR)/obj
OBJ=$(ODIR)/main.o $(ODIR)/point.o $(ODIR)/traingle.o

opengldemo: $(OBJ)
	$(CC) $(DBGFLAGS) $(CFLAGS) $(BUILDDIR)/bin/opengldemo $(LIBS) $(FWORKS) $^
	
$(ODIR)/%.o: app/%.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o $@ $<
```



I can also use makefiles to define simple tasks like removing or making directories, etc. These tasks are defined as targets and the actual command to do the task as the build command.

```makefile
setup:
	mkdir -p $(BUILDDIR)/bin
	mkdir -p $(ODIR)
	
clean:
	rm -fr $(BUIDDIR)
```

Now I can use these targets as depdencies to other targets -

```makefile
opengldemo: $(OBJ) setup
	...
```

Or even define a new target with no commands -

```makefile
build: clean setup
```

An edge case here can be if the directory tree contains a file named `clean` or `setup`, make will be confused as to whether the depedency is referring to a target within this makefile or to the actual file. To disambiguate this and tell make that this is referring to the targets defined here, we use the keyword `.PHONY`.

```makefile
.PHONY: clean setup build
```

Now the final makefile looks like -

```makefile
CC=clang++
CFLAGS=-std=c++20 -Wall -I.
DBGFLAGS=-g
HEADERS=utils/Point.h utils/Triangle.h
LIBS=-lglfw.3 -lGLEW.2.1
FWORKS=-framework OpenGL -framework Cocoa -framework IOKit
BUILDDIR=.
ODIR=$(BUILDDIR)/obj
OBJ=$(ODIR)/main.o $(ODIR)/point.o $(ODIR)/traingle.o

opengldemo: $(OBJ)
	$(CC) $(DBGFLAGS) $(CFLAGS) $(BUILDDIR)/bin/opengldemo $(LIBS) $(FWORKS) $^
	
$(ODIR)/%.o: app/%.cpp $(HEADERS)
	$(CC) -c $(DBGFLAGS) $(CFLAGS) -o $@ $<

.PHONY: setup clean build

setup:
	mkdir -p $(BUILDDIR)/bin
	mkdir -p $(ODIR)
	
clean:
	rm -fr $(BUIDDIR)
  
build: clean setup opengldemo  
```

## CMake

### Building a Shared Library

Lets take the simple directory structure we saw earlier. Add `CMakeLists.txt`.

```
aptg-stuff/
        stuff.h
        stuff.cc
        CMakeLists.txt
```

The contents of this file are:

```cmake
cmake_minimum_required(VERSION 3.19)

project(AptgStuff VERSION 0.1)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_library(AptgStuff SHARED stuff.cc)

install(TARGETS AptgStuff DESTINATION lib/aptg)
install(FILES stuff.h DESTINATION include/aptg)
```

> `project`, `set`, `add_library`, `install`, etc. are called [CMake commands](https://cmake.org/cmake/help/latest/manual/cmake-commands.7.html). `CMAKE_CXX_STANDARD` is a [CMake variable](https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html)

For C++ projects I don't have to explicitly specify the language, but for C projects I need to as part of the `project` command like so -

```
project(AptgStuff VERSION 0.1 LANGUAGES C)
```

First CMake needs to be configured -

```shell
cmake -S . -B build
```

`-S` specifies the source directory. This means that any relative paths in the CMakeLists.txt will be rooted here. `-B` specifies the build directory. This tells CMake to create a directory called `build` and configure CMake to create all the make artifacts there.

Then we actually build the code -

```shell
cmake --build build
```

This will create a `libAptgStuff.dylib` in the `build` directory. The shared library created by CMake will be s.t that when we link it to an executable, the path is set as `@rpath/libAptgStuff.dylib`.  After this done we need to "install" the header files and the binary.

```shell
cmake --install build
```

This will copy `stuff.h` to `/usr/local/include/aptg` and `libAptgStuff.dylib` to `/usr/local/lib/aptg`. 

To configure a debug build do -

```shell
cmake -S . -B build/debug -D CMAKE_BUILD_TYPE=Debug
```

This will automatically generate makefiles with the right debug flags (e.g., `-g`) for the current compiler.  Now I can build as per usual -

```shell
cmake --build build/debug
```

Similarly for release builds do -

```shell
cmake -S . -B build/release -D CMAKE_BUILD_TYPE=Release
cmake --build build/release
```

Another good flag to use is the `CMAKE_EXPORT_COMPILE_COMMANDS` which will generate a `compile_commands.json` file with the exact compiler calls for all translation units. This will not show the linker commands. AFAIK - there is no equivalent of showing linker commands.

```shell
cmake -S . -B build -D CMAKE_BUILD_TYPE=DEBUG -D CMAKE_EXPORT_COMPILE_COMMANDS=YES
```

To set specific compiler flags like `-Wall`, I need to use `set(CMAKE_CXX_FLAGS "-Wall")`. While I can set this anywhere, convention is for these types of settings to be set in a separte "toolchain" file and give CMake the path to this file when configuring it. E.g., 

Contents of `toolchain.cmake`:

```cmake
set(CMAKE_C_FLAGS "-Wall")
```

Now when I configure CMake I say -

```shell
cmake -S . -B build --toolchain ./toolchain.cmake
```

There is also another similar variable called `CMAKE_<LANG>FLAGS_DEBUG`. If I set it to `-Wall` then the `CMAKE_BUILD_TYPE=Debug` will not have any effect, because the `-g` option will be overridden by what I have specified as my debug flags, and I have not specified `-g` in it!

This flag will be applied to all source files, even ones that I have not authored! To warn on only some targets use -

```cmake
target_compile_options(my_target PRIVATE -Wall)
```

There is another CMake flag called `CMAKE_COMPILE_WARNING_AS_ERROR` that I can set in either the toolchain or presets file.

### With VSCode

==TODO: Using preset==

If I want to manually set my compiler and I am using both a preset and a toolchain file, it is best to set the `CMAKE_C_COMPILER` in the preset's cache instead of toolchain. This will help with debugging.

### Building an Executable

Lets say I am building an executable that uses an external library that has no special support for CMake. E.g., the library we built above does not have any special support for CMake, even though we built it using CMake.

As usual we add a `CMakeLists.txt` to the app root.

```
aptg-app/
        main.cc
        CMakeLists.txt
```

Here are the contents of `CMakeLists.txt`. 

```cmake
cmake_minimum_required(VERSION 3.19)

project(AptgApp VERSION 0.1)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_library(AptgStuff SHARED IMPORTED)

set_property(TARGET AptgStuff PROPERTY
             IMPORTED_LOCATION "/usr/local/lib/aptg/libAptgStuff.dylib")

# The following ways to set properties are all equivalent
# set_target_properties(AptgStuff PROPERTIES
#                       INTERFACE_INCLUDE_DIRECTORIES "/usr/local/include/aptg"
# )

# set_property(TARGET AptgStuff 
#              PROPERTY INTERFACE_INCLUDE_DIRECTORIES 
#              "/usr/local/include/aptg")

target_include_directories(AptgStuff INTERFACE "/usr/local/include/aptg")

add_executable(AptgApp main.cpp norm.cpp)
target_link_libraries(AptgApp PRIVATE AptgStuff)
```

The first few lines are standard. The interesting stuff happens when we `add_library`. The `SHARED` keyword says this is a dynamically linked library, and the `IMPORTED` keyword says that this is an external library. No special makefiles will be generated for this target. Next we have to tell CMake where to get the artifacts for this target, since it is not building it. So we set a TARGET property called `IMPORTED_LOCATION` where we specify the full path of the library. Next we setup the include directories s.t any other target that links to this library will automatically have these include paths added. Next we link the executable to this library target just like before. 

Building this will create an executable with the relpaths configured. Running the executable "just works".

### Building a Shared Library with CMake Support

When importing an external library with no CMake support, I have to know where the libary is installed, the header files that I need to include, etc. However, if I build a library with CMake support, using it in another CMake project becomes easier. Here is how to write the CMakeLists.txt file for such a library.

```cmake
cmake_minimum_required(VERSION 3.19)

project(AptgStuff VERSION 0.1)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_library(AptgStuff SHARED stuff.cc)
target_include_directories(AptgStuff
                           PUBLIC
                           "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
                           "$<INSTALL_INTERFACE:include/aptg>"
)

install(TARGETS AptgStuff
        EXPORT AptgStuffTargets
        LIBRARY DESTINATION lib/aptg
        ARCHIVE DESTINATION lib/aptg
        RUNTIME DESTINATION bin/aptg
        INCLUDES DESTINATION include/aptg
)
install(FILES stuff.h DESTINATION include/aptg)
install(EXPORT AptgStuffTargets
        FILE AptgStuffTargets.cmake
        NAMESPACE AptgStuff::
        DESTINATION lib/aptg/cmake
)
```

Here are a few things to note:

1. The `target_include_directories` has the include paths for both building the library itself (`$<BUILD_INTERFACE:`) and when this library is used from the "install" path (`$<INSTALL_INTERFACE:`). 
2. Usually the `install(TARGETS)` just has the `RUNTIME DESTINATION`, but here we have the `EXPORT` argument and a bunch of other things that go with exporting a package.
3. A new `install(EXPORT)` directive tells the CMake install command to create a `AptgStuffTargets.cmake` file that a client project can simply include and get all the paths set up automagically. The `NAMESPACE` option is a CMake namespace, not to be confused with a C++ namespace. This supposedly helps CMake print better diagnostic messages. When linking against this library I'll have to use `AtpgStuff::AptgStuff`.

### Using a Shared Library with CMake Support

The `CMakeLists.txt` for such an executable looks like -

```cmake
cmake_minimum_required(VERSION 3.19)

project(AptgApp VERSION 0.1)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

include(/usr/local/lib/aptg/cmake/AptgStuffTargets.cmake)
add_executable(AptgApp main.cpp norm.cpp)
target_link_libraries(AptgApp PRIVATE AptgStuff::AptgStuff)
```

Here we first include the `AptgStuffTargetes.cmake` file that the library should have installed. This sets up the `add_library` target and the `target_include_directories` directive. All I have to do then is to link to this library.

### Building a Full CMake Package

When using the library we built above, we still have to include the full path of the `AptgStuffTargets.cmake` file. If we want to have version compatibility, we have to check the version some other way. This is where CMake packages come in handy. By creating and installing two more files `AptgStuffConfig.cmake` and `AptgStuffConfigVersion.cmake`, we make it super easy for clients to install CMake packages and do version checking. We don't have to author the `AptgConfigVersion.cmake` and `AptgStuffConfigVersion.cmake` by hand. There are CMake "macros" that do this. All we need to do is to specify a `.in` file that will be taken in by a macro similar to `configure_file` and will output the full `Config.cmake` file. This macro is called `configure_package_config_file`. The `ConfigVersion.cmake` can similarly be generated by calling a `write_basic_package_version_file` macro. Here is what the full CMakeLists.txt looks like - 

```cmake
cmake_minimum_required(VERSION 3.19)

set(version 3.4.1)
project(AptgStuff VERSION ${version})

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_library(AptgStuff SHARED stuff.cc)
target_include_directories(AptgStuff
                           PUBLIC
                           "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>"
                           "$<INSTALL_INTERFACE:include/aptg>"
)

install(TARGETS AptgStuff
        EXPORT AptgStuffTargets
        LIBRARY DESTINATION lib/aptg
        ARCHIVE DESTINATION lib/aptg
        RUNTIME DESTINATION bin/aptg
        INCLUDES DESTINATION include/aptg
)
install(FILES stuff.h DESTINATION include/aptg)
install(EXPORT AptgStuffTargets
        FILE AptgStuffTargets.cmake
        NAMESPACE AptgStuff::
        DESTINATION lib/aptg/cmake
)

include(CMakePackageConfigHelpers)

set_property(TARGET AptgStuff PROPERTY VERSION ${version})
set_property(TARGET AptgStuff PROPERTY SOVERSION 3)
set_property(TARGET AptgStuff PROPERTY
  INTERFACE_AptgStuff_MAJOR_VERSION 3)
set_property(TARGET AptgStuff APPEND PROPERTY
  COMPATIBLE_INTERFACE_STRING AptgStuff_MAJOR_VERSION
)

# generate the version file for the config file
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/AptgStuffConfigVersion.cmake"
  VERSION "${version}"
  COMPATIBILITY AnyNewerVersion
)

configure_package_config_file(${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/AptgStuffConfig.cmake"
  INSTALL_DESTINATION lib/aptg/cmake/AptgStuff
)

install(FILES
            "${CMAKE_CURRENT_BINARY_DIR}/AptgStuffConfig.cmake"
            "${CMAKE_CURRENT_BINARY_DIR}/AptgStuffConfigVersion.cmake"
        DESTINATION lib/aptg/cmake
)
```

The interesting stuff starts with the `include(CMakePackageConfigHelpers)` line. But before that I am now storing the version in a variable called `version`. This will be used when generating my `ConfigVersion.cmake` file. The call to `write_basic_package_version_file` writes a `AptgStuffConfigVersion.cmake` file to the local `build` directory. 

The `configure_package_config_file` takes an input file `Config.cmake.in` in the source directory and generates a `AptgStuffConfig.cmake` in the local build dir. We also specify the install destination here. The main statements in this file are including the `AptgStuffTargets.cmake` and checking the version.

Now both the generated files are installed in `${CMAKE_INSTALL_PREFIX}/lib/aptg/cmake` along with `AptgStuffTargets.cmake`. 

Here is what the Config.cmake.in file looks like -

```cmake
@PACKAGE_INIT@

include("${CMAKE_CURRENT_LIST_DIR}/AptgStuffTargets.cmake")

check_required_components(AptgStuff)
```

Running a `cmake --install build` will install the following files - 

```shell
$ cmake --install build
-- Install configuration: "Debug"
-- Installing: /usr/local/lib/aptg/libAptgStuff.3.4.1.dylib
-- Installing: /usr/local/lib/aptg/libAptgStuff.3.dylib
-- Installing: /usr/local/lib/aptg/libAptgStuff.dylib
-- Installing: /usr/local/include/aptg/stuff.h
-- Installing: /usr/local/lib/aptg/cmake/AptgStuffTargets.cmake
-- Installing: /usr/local/lib/aptg/cmake/AptgStuffTargets-debug.cmake
-- Installing: /usr/local/lib/aptg/cmake/AptgStuffConfig.cmake
-- Installing: /usr/local/lib/aptg/cmake/AptgStuffConfigVersion.cmake
```

### Using a CMake Package

The CMakeLists.txt of such an executable is even simpler than before.

```cmake
cmake_minimum_required(VERSION 3.19)

project(AptgApp VERSION 0.1)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(AptgStuff_DIR "/usr/local/lib/aptg/cmake")
find_package(AptgStuff 3.4.1 EXACT)

add_executable(AptgApp main.cpp norm.cpp)
target_link_libraries(AptgApp PRIVATE AptgStuff::AptgStuff)
```

Specify the directory where CMake will find the `AptgStuffConfig.cmake` file and then find that package. After that it is the usual linking.

### CMake Usage Examples

Usual CMAKE settings that I like to set -

```cmake
-DCMAKE_BUILD_TYPE=Debug 
-DCMAKE_C_COMPILER=gcc 
-DCMAKE_EXPORT_COMPILE_COMMANDS=YES
-DCMAKE_C_FLAGS=-Wall
```

#### Using `libm` for math functions

```cmake
cmake_minimum_required(VERSION 3.30)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

project(demomath, VERSION 0.1 LANGUAGES C)

# This is not needed because libm is a standard library?
# add_library(m SHARED IMPORTED)
# set_property(TARGET m PROPERTY IMPORTED_LOCATION "/usr/lib/libm.so")
# target_include_directories(m INTERFACE "/usr/include")

add_executable(demomath main.c)
target_link_libraries(erf PRIVATE m)

# The corresponding gcc build command is -
# gcc ... -lm src/erf.c
```

#### Using libraries with `pkg-config` compatibility

An example of such a library is `glib-2.0`. It does not have a cmake package, but rather relies on `pkg-config` to find all the compiler and linker flags.

```cmake
cmake_minimum_required(VERSION 3.30)

set(CMAKE_C_STANDARD 17)

project(demoglib2 VERSION 0.1 LANGUAGES C)

find_package(PkgConfig REQUIRED)
# pkg_check_modules can get multiple packages at once in the same variable
pkg_check_modules(glib2 REQUIRED IMPORTED_TARGET glib-2.0)

add_executable(demoglib2 main.c)
target_link_libraries(demoglib2 PkgConfig::glib2)
```

#### Adding resource files to the build directory

Lets say I have a `./resources/` directory with a bunch of different resources that I want in the build directory. These don't change frequently so I am ok with a one-time copy.

```cmake
file(COPY ${CMAKE_SOURCE_DIR}/resources DESTINATION ${CMAKE_BINARY_DIR})
```

The above command will copy the files at config time.

However, if these files are changing frequently, then running config everytime can be annoying.

```cmake
set(RESOURCE_FILES resources/file1.png resources/file2.png)

FOREACH(RESOURCE_FILE ${RESOURCE_FILES})
	add_custom_command(
		OUTPUT ${CMAKE_BINARY_DIR}/${RESOURCE_FILE}
		COMMAND ${CMAKE_COMMAND} -E copy
			${CMAKE_SOURCE_DIR}/${RESOURCE_FILE}
			${CMAKE_BINARY_DIR}/${RESOURCE_FILE}
		DEPENDS ${CMAKE_SOURCE_DIR}/${RESOURCE_FILE}
	)
ENDFOREACH()

list(TRANSFORM RESOURCE_FILES PREPEND ${CMAKE_BINARY_DIR}/)
add_executable(${MAIN_TARGET} ${RESOURCE_FILES} main.cpp)
```

#### Adding a compile definition

Lets say I want to add a compile definition s.t. I can do `#ifdef PLATFORM_WEB` in my code. Normally in `gcc` I'd just say `gcc -DPLATFORM_WEB main.c -o main.out` or equivalent. In CMake I can do this in two different ways - 

* One is just specify the additional c flag in the CMakeLists.txt as `set(CMAKE_C_FLAGS "-DPLATFORM_WEB" )`.
* Another is to use `target_compile_definitions(${PROJECT_NAME} PUBLIC -DPLATFORM_WEB)` after I have defined the `add_executable`. 

## Debugging

In order to debug with VSCode, the debugger needs to know how to launch the process that is going to be debugged. If there is a default build task defined already it will use that. For simple projects or single file programs I can define the default task by `cmd+shift+p` and typing "Tasks: Configure Default Build Task". This will show me a dropdown, choose the "clang++" dropdown. The auto-generated tasks.json is good enough for debugging. Just start debugging. There is no need to create a custom launch.json or anything like that.

For a bigger project, lets say I have the following directory structure for a project -

```
bookmarker
├── Makefile
├── Main.cpp
├── Utils.cpp
└── Utils.h
```

And lets say that `make debug` will build the debug build as `~/Desktop/temp/bookmarker/bookmarker`.

> I'll need to use the following debug flags when building -
>
> `-fcolor-diagnostics -fansi-escape-codes -g`

In order to debug with VSCode I need to create the following files -

```
bookmarker/.vscode/launch.json
bookmarker/.vscode/tasks.json
```

In `tasks.json` I need to define the build command that will generate the debug build. Here is what it can look like -

```json
{
  "tasks": [
    {
      "type": "cppbuild",
      "label": "C/C++: clang++ build active file",
      "command": "make",
      "args": [
        "debug"
      ],
      "options": {
        "cwd": "${fileDirname}"
      },
      "problemMatcher": ["$gcc"],
      "group": {
        "kind": "build",
        "isDefault": true
      },
      "detail": "Task generated by debugger."
    }
  ],
  "version": "2.0.0"
}
```

In `launch.json` I need to specify the location of the debug build to launch and the debugger to use. 

Here is what it can look like on MacOS -

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "name": "Debug Bookmarker",
      "type": "cppdbg",
      "request": "launch",
      "program": "/Users/avilay/Desktop/temp/bookmarker/bookmarker",
      "stopAtEntry": false,
      "cwd": ".",
      "osx": {
        "MIMode": "lldb"
      }
    }
  ]
}
```

And here is what it can look like on Linux -

```json
{
    "version": "0.2.0",
    "configurations": [
      {
        "name": "Debug cookie",
        "type": "cppdbg",
        "request": "launch",
        "program": "/home/avilay/projects/bitbucket/learn/learn-lang/learn-c/cookie/bin/app",
        "stopAtEntry": false,
        "cwd": ".",
        "MIMode": "gdb",
        "miDebuggerPath": "/usr/bin/gdb",
      }
    ]
  }
```

> It is possible to debug clang compiled programs with gdb as long as I am generating the debug symbols via the `-g` flag.

Now when I click on the debug button, I'll be given the option to choose the launch configuration I want to use, in my case there is just one, clicking on that will launch the debugger. See [Configure launch.json for C/C++ debugging in Visual Studio Code](https://code.visualstudio.com/docs/cpp/launch-json-reference) for more details.







