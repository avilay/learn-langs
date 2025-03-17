cmake_minimum_required(VERSION 3.30)
project(learn_catch2_1)

set(CMAKE_CXX_STANDARD 20)

add_executable(${PROJECT_NAME}
        main.cpp
        Cookie.cpp
        Cookie.h
)

find_package(Catch2 3 REQUIRED)
add_executable(${PROJECT_NAME}-tests
        Cookie.cpp
        Cookie.h
        test_Cookie.cpp)
target_link_libraries(${PROJECT_NAME}-tests PRIVATE Catch2::Catch2WithMain)
include(Catch)
catch_discover_tests(${PROJECT_NAME}-tests)
