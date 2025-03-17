cmake_minimum_required(VERSION 3.28)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(second VERSION 1.0)

# I can either build libblah.a here -
# add_library(blah STATIC blah/Blah.cpp)
# target_include_directories(blah PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/blah/include)
# - OR -
# Create a CMakeLists.txt inside the blah directory and call it here.
add_subdirectory(blah)

file(GLOB_RECURSE SRC_FILES src/*.cpp)
add_executable(second ${SRC_FILES})
target_link_libraries(second PUBLIC blah)
