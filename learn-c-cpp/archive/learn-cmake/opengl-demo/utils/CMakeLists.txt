add_library(ShapeUtils Point.cpp Triangle.cpp)
target_include_directories(ShapeUtils INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
