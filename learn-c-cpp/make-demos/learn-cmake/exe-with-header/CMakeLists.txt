cmake_minimum_required(VERSION 3.30)
project(exe-with-header C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} src/main.c include/blah.h src/blah.c)
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
