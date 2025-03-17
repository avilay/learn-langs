cmake_minimum_required(VERSION 3.19)

project(LearnOpenGL VERSION 0.1)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_library(Glew SHARED IMPORTED)
set_property(TARGET Glew PROPERTY
             IMPORTED_LOCATION "/opt/homebrew/Cellar/glew/2.2.0_1/lib/libGLEW.dylib")
target_include_directories(Glew INTERFACE "/opt/homebrew/Cellar/glew/2.2.0_1/include")

add_library(Glfw SHARED IMPORTED)
set_property(TARGET Glfw PROPERTY
             IMPORTED_LOCATION "/opt/homebrew/Cellar/glfw/3.3.3/lib/libglfw.dylib")
target_include_directories(Glfw INTERFACE "/opt/homebrew/Cellar/glfw/3.3.3/include")

find_library(OPEN_GL_FW OpenGL)
find_library(COCOA_FW Cocoa)
find_library(IOKIT_FW IOKit)

add_subdirectory(utils)
add_subdirectory(app)
