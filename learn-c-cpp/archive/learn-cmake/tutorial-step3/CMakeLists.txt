# Almost as same as Step2. The only difference is that I no longer have to specify
# the MathFunctions/MathFunctions.h in include directories. The reason for this lies
# in the MathFunctions/CMakeLists.txt. It runs a command called target_include_directories
# which tells the cmake system that any target that links to this library should have
# the specified header file automatically added to its include path.
cmake_minimum_required(VERSION 3.17)

project(Tutorial VERSION 1.0)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

option(USE_MYMATH "Use tutorial provided math implementation" ON)

configure_file(TutorialConfig.h.in TutorialConfig.h)

if (USE_MYMATH)
	add_subdirectory(MathFunctions)
	list(APPEND EXTRA_LIBS AptgMathFunctions)
endif()

add_executable(Tutorial tutorial.cxx)

target_link_libraries(Tutorial PUBLIC ${EXTRA_LIBS})
target_include_directories(
	Tutorial PUBLIC 
    "${PROJECT_BINARY_DIR}"
)
