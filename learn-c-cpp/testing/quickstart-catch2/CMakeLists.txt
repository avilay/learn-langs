cmake_minimum_required(VERSION 3.30)
project(quickstart_catch2)

set(CMAKE_CXX_STANDARD 20)

add_executable(quickstart_catch2 main.cpp
        factorial_impl.cpp
        factorial_impl.h)

find_package(Catch2 3 REQUIRED)
add_executable(unit-tests
        factorial_impl.cpp
        factorial_impl.h
        test_factorial.cpp
)
target_link_libraries(unit-tests PRIVATE Catch2::Catch2WithMain)
include(Catch)
catch_discover_tests(unit-tests)

