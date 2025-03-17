# CMake

## Basics

The Makefile equivalent in CMake is `CMakeLists.txt`. This is usually at the top level of the project directory.

```
aptg-stuff/
	stuff.h
	stuff.c
	CMakeLists.txt
```

CMake has a bunch of [CMake commands](https://cmake.org/cmake/help/latest/manual/cmake-commands.7.html) and a bunch of pre-defined [CMake variables](https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html).

All CMakeLists.txt files start with -

```cmake
cmake_minimum_required(VERSION 3.30)
project(AptgStuff VERSION 0.1)
```

The first line defines the minimum cmake version needed to make this project. CMake seems to have a bunch of breaking changes with new versions so it is good to pin this down. The next line is the name of my project. For C++ projects I don't need to specify the language of this project, but for all other languages I need to specify it -

```cmake
project(AptgStuff VERSION 0.1 LANGUAGES C)
```

This is usually followed by the equivalent of `-std=c++20` or `-std=gnu17` flags -

```cmake
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
```

or in case of C

```cmake
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)
```

`project`, `set`, etc. are CMake commands, and `CMAKE_CXX_STANDARD` is an example of a CMake variable.



## Use Cases

In all the following use cases use the following command to configure and build -

```shell
cmake -DCMAKE_BUILD_TYPE=Debug \
      -DCMAKE_C_COMPILER=gcc \
      -DCMAKE_EXPORT_COMPILE_COMMANDS=YES \
      -DCMAKE_C_FLAGS=-Wall \
      -S . -B build
      
cmake --build build      
```

When showing the equivalent gcc command I'll skip the usual debug and warning flags, so my complete gcc command will be something like -

```shell
gcc -g -Wall -std=gnu17 <all other stuff>
```

### A basic executable with no external dependencies

```
aptg-executable/
├── CMakeLists.txt
└── main.c
```

```cmake
cmake_minimum_required(VERSION 3.30)
project(scratch C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(scratch main.c)
```

### An executable with header files

```
exe-with-header/
├── CMakeLists.txt
├── include
│   └── blah.h
└── src
├── blah.c
└── main.c
```

```cmake
cmake_minimum_required(VERSION 3.30)
project(exe-with-header C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} src/main.c include/blah.h src/blah.c)
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
```

### A simple library

```
simple-lib/
├── CMakeLists.txt
├── include
│   └── blah.h
└── src
└── blah.c
```

```cmake
cmake_minimum_required(VERSION 3.30)
project(simple C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

# To build a static library say -
# add_library(${PROJECT_NAME} STATIC ...)
# STATIC is default so it can be omitted
add_library(${PROJECT_NAME} SHARED include/blah.h src/blah.c)
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

install(TARGETS ${PROJECT_NAME} DESTINATION /usr/local/lib)
install(FILES include/blah.h DESTINATION /usr/local/include/simple)
```

To install the build -

```shell
sudo cmake --install build
-- Install configuration: "Debug"
-- Installing: /usr/local/lib/simple/libsimple.a
-- Installing: /usr/local/include/simple/blah.h
```

### An executable dependent on a library

```shell
exe-with-dep-lib/
├── CMakeLists.txt
└── main.c
```

Depending on whether the library and its header are in a well-known location or not, we need different cmake files. To know the well-known locations for libraries do -

```shell
gcc -print-search-dirs
```

On my Arch box I get -

```shell
ॐ string split : (gcc -print-search-dirs | grep libraries)                                                                                                         (base)
libraries
=/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/
/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/../../../../x86_64-pc-linux-gnu/lib/x86_64-pc-linux-gnu/14.2.1/
/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/../../../../x86_64-pc-linux-gnu/lib/../lib/
/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/../../../x86_64-pc-linux-gnu/14.2.1/
/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/../../../../lib/
/lib/x86_64-pc-linux-gnu/14.2.1/
/lib/../lib/
/usr/lib/x86_64-pc-linux-gnu/14.2.1/
/usr/lib/../lib/
/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/../../../../x86_64-pc-linux-gnu/lib/
/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/../../../
/lib/
/usr/lib/
```

The thing to note is that `/usr/lib` is a well-known directory, but `/usr/local/lib` is not.

Similarly to get the well-known include paths do -

```shell
echo | gcc -xc -E -v -
```

* `-x` selects the language - C or C++
* `-E` makes gcc run the pre-processor only, so no compilation
* `-v` prints all the commands run, which is the key to dumping the standard paths
* `-` make gcc get the input source file from stdin
* `echo` gives an empty input file

On my Arch box I get -

```shell
<snip>
#include "..." search starts here:
#include <...> search starts here:
/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/include
/usr/local/include
/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/include-fixed
/usr/include
</snip>
```

Here I can see that both `/usr/local/include` and `/usr/include` are well-known locations.

#### Scenario 1: library in well-known location

My library/headers are in well-known locations -

```shell
/usr/lib/libsimple.so
/usr/include/blah.h
```

The gcc command is -

```shell
gcc -lsimple -o scratch main.c
```

The equivalent cmake file is -

```cmake
cmake_minimum_required(VERSION 3.30)
project(exe-with-dep-lib C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} main.c)

target_link_libraries(${PROJECT_NAME} PRIVATE simple)
```

#### Scenario 2: library not in well-known location

My library/headers are **not** in well-known locations  -

```shell
/usr/local/lib/libsimple.so
/usr/local/include/simple/blah.h
```

The header file is also not in a well known path. Even though the header file is in a subdir of a well-known path, subdirs are not considered well-known. The gcc command is -

```shell
gcc -g -Wall -std=gnu17 -lsimple -L/usr/local/lib -Wl,-rpath,/usr/local/lib -I/usr/local/include/simple -o scratch main.c
```

The equivalent cmake file is -

```cmake
cmake_minimum_required(VERSION 3.30)
project(exe-with-dep-lib C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} main.c)

target_include_directories(${PROJECT_NAME} PUBLIC /usr/local/include/simple)
target_link_directories(${PROJECT_NAME} PUBLIC /usr/local/lib/)
target_link_libraries(${PROJECT_NAME} PRIVATE simple)
```

If I move the header file up one level to `/usr/local/include` then I don't need the `-I` flag -

```shell
gcc -g -Wall -std=gnu17 -lsimple -L/usr/local/lib -Wl,-rpath,/usr/local/lib -o scratch main.c
```

Similarly in the cmake file no need to `target_include_directories` -

```cmake
cmake_minimum_required(VERSION 3.30)
project(exe-with-dep-lib C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} main.c)

target_link_directories(${PROJECT_NAME} PUBLIC /usr/local/lib/)
target_link_libraries(${PROJECT_NAME} PRIVATE simple)
```

#### Scenario 3: library with cmake support

The library was installed as a cmake package. The cmake config will usually be in `/usr/lib/cmake`. I can get the version of the package from the `<pkgname>-config-version.cmake` file found in the `/usr/lib/cmake/<pkg-name>` directory. Lets say I am using `json-c`, which does not have a version specified. Here is what my executable's cmake file will look like -

```cmake
cmake_minimum_required(VERSION 3.30)
project(exe-with-dep-cmake C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} main.c)

find_package(json-c)

target_link_libraries(${PROJECT_NAME} PRIVATE json-c)
```

If I knew the version number of the package, I can issue the following `find_package` command -

```cmake
find_package(raylib 3.0 REQUIRED)
# or
find_package(raylib 4.5.0 EXACT)
```

#### Scenario 4: library with pkg-config support

My library/headers are obtained with `pkg-config`. [`glib2.0`](https://docs.gtk.org/glib/) is such a package. It can be installed on Arch with `pacman -S glib2`. 

The output of `pkg-config` is -

```shell
ॐ pkg-config --cflags --libs glib-2.0
-I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include -I/usr/include/sysprof-6 -pthread -lglib-2.0
```

The gcc command (on bash) will be -

```shell
gcc -o scratch main.c `pkg-config --cflags --libs glib-2.0`
```

On fish I'll have to do -

```fish
eval gcc -o scratch main.c (pkg-config --cflags --libs glib-2.0)
```

My code dir -

```shell
exe-with-dep-pkgconfig/
├── CMakeLists.txt
└── main.c
```

cmake file -

```cmake
cmake_minimum_required(VERSION 3.30)
project(exe-with-dep-pkgconfig C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} main.c)

find_package(PkgConfig REQUIRED)
pkg_check_modules(glib2 REQUIRED IMPORTED_TARGET glib-2.0)

target_link_libraries(${PROJECT_NAME} PRIVATE PkgConfig::glib2)
```

### Adding resource files to the build directory

Lets say I have a `./resources/` directory with a bunch of different resources that I want in the build directory. These don't change frequently so I am ok with a one-time copy.

```cmake
file(COPY ${CMAKE_SOURCE_DIR}/resources DESTINATION ${CMAKE_BINARY_DIR})
```

Lets say I have the following dir tree -

```shell
my-project
  resources
    logo.png
    greet.wav
  main.c
```

The above command will create a directory called `resources` in the build dir and copy all the files there.

```shell
my-project
  build
    resources
      logo.png
      greet.wav
```

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

### Adding a compile definition

Lets say I want to add a compile definition s.t. I can do `#ifdef PLATFORM_WEB` in my code. Normally in `gcc` I'd just say `gcc -DPLATFORM_WEB main.c -o main.out` or equivalent. In CMake I can do this in two different ways - 

* One is just specify the additional c flag in the CMakeLists.txt as `set(CMAKE_C_FLAGS "-DPLATFORM_WEB" )`.
* Another is to use `target_compile_definitions(${PROJECT_NAME} PUBLIC -DPLATFORM_WEB)` after I have defined the `add_executable`. 
