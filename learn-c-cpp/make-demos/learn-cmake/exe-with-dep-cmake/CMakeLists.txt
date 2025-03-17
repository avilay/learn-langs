cmake_minimum_required(VERSION 3.30)
project(exe-with-dep-cmake C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} main.c)

find_package(json-c)

target_link_libraries(${PROJECT_NAME} PRIVATE json-c)