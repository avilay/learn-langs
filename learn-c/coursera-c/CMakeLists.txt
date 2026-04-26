cmake_minimum_required(VERSION 4.0)
project(coursera_c C)

set(CMAKE_C_STANDARD 17)

add_executable(alloc alloc_main.c alloc.h alloc.c)
add_executable(concat concat.c)
add_executable(scratch scratch.c)