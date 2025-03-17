cmake_minimum_required(VERSION 3.28)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(third VERSION 1.0)

find_package(docopt COMPONENTS CXX REQUIRED)
include_directories(${DOCOPT_INCLUDE_DIRS})

add_executable(third Main.cpp)
target_link_libraries(third docopt)