# Build Stuff

## Basic

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
```

This will create the `libAptgStuff.dylib` file in `aptg-stuff`. Typically I want to "install" this library in some well known location.

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

### Linker Paths

A weird thing that happens is that the `aptgapp` executable expects to find the `libAptgStuff.dylib` in its local directory. The linker didn't save the `-L` path in the executable. To see the exact paths where the executable expects the linked libraries, `otool` is a prettty useful utility. Running it on `aptgapp` gives us -

```shell
$ otool -L aptgapp 
aptgapp:
        libAptgStuff.dylib (compatibility version 0.0.0, current version 0.0.0)
        /usr/lib/libc++.1.dylib (compatibility version 1.0.0, current version 904.4.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1292.60.1)
```

I can change the paths directly in the executable using another very useful tool called `install_name_tool`. Running the `otool` utility again shows the right path.

```shell
$ install_name_tool -change libAptgStuff.dylib /usr/local/lib/libAptgStuff.dylib aptgapp 
$ otool -L aptgapp 
aptgapp:
        /usr/local/lib/libAptgStuff.dylib (compatibility version 0.0.0, current version 0.0.0)
        /usr/lib/libc++.1.dylib (compatibility version 1.0.0, current version 904.4.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1292.60.1)
```

Of course the easier thing to do is to just copy the `libAptgStuff.dylib` to the same directory as the executable.

If I build my library using CMake, it adds an `@rpath` in the library such that when I build my executable, the path of the library shows up as `@rpath/libCMakeAptgStuff.dylib`. This seems like a MacOS specific thing. Some blogpost said that it can be set with the environment variable `LD_RUNPATH_SEARCH_PATH` but when I tried it, it did not work. Another way to specify the `rpath` is via a linker option.

```shell
clang++ -std=c++17 -o aptgapp -L/usr/local/lib/aptg -lAptgStuff -Wl,-rpath,/usr/local/lib -I/usr/local/include main.cc
```

The `-Wl` switch tells `clang++` to pass the rest of the CSV string to the linker. This bakes the rpath in the executable.

## With CMake

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

First CMake needs to be configured -

```shell
cmake -Bbuild
```

This tells CMake to create a directory called `build` and configure CMake to create all the make artifacts there.

Then we actually build the code -

```shell
cmake --build build
```

This will create a `libAptgStuff.dylib` in the `build` directory. The shared library created by CMake will be s.t that when we link it to an executable, the path is set as `@rpath/libAptgStuff.dylib`.  After this done we need to "install" the header files and the binary.

```shell
cmake --install build
```

This will copy `stuff.h` to `/usr/local/include/aptg` and `libAptgStuff.dylib` to `/usr/local/lib/aptg`. 

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





