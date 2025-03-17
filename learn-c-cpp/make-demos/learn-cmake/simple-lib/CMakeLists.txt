cmake_minimum_required(VERSION 3.30)
project(simple C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

# To build a static library say -
# add_library(${PROJECT_NAME} STATIC ...)
# STATIC is default so it can be omitted
# add_library(${PROJECT_NAME} SHARED include/blah.h src/blah.c)
add_library(${PROJECT_NAME} STATIC include/blah.h src/blah.c)
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

install(TARGETS ${PROJECT_NAME} DESTINATION /home/avilay/temp)
install(FILES include/blah.h DESTINATION /home/avilay/temp)
