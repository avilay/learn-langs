cmake_minimum_required(VERSION 3.30)

# This file is equivalent to the following command -
# clang++ -std=c++20 -I./include src/Main.cpp src/Blah.cpp

# project(first VERSION 1.0)
project(first C)

# set(CMAKE_CXX_STANDARD 20)
# set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)


# The "correct" way of adding source files is explicitly.
# add_executable(first src/Main.cpp src/Blah.cpp)

# An alternate way is to glob all *.cpp files inside src
file(GLOB_RECURSE SRC_FILES src/*.c)
add_executable(first ${SRC_FILES})

target_include_directories(first PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
