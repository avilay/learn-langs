cmake_minimum_required(VERSION 3.29)

set(CMAKE_C_STANDARD 17)
set (CMAKE_C_STANDARD_REQUIRED ON)

project(hello-world VERSION 0.1 LANGUAGES C)

add_executable(hello-world src/main.c src/utils.c)

