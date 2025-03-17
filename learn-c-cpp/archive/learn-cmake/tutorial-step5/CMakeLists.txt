cmake_minimum_required(VERSION 3.17)

project(Tutorial VERSION 1.0)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

option(USE_MYMATH "Use tutorial provided math implementation" ON)

configure_file(TutorialConfig.h.in TutorialConfig.h)

if (USE_MYMATH)
	add_subdirectory(MathFunctions)
	list(APPEND EXTRA_LIBS MathFunctions)
endif()

add_executable(Tutorial tutorial.cxx)

target_link_libraries(Tutorial PUBLIC ${EXTRA_LIBS})
target_include_directories(
	Tutorial PUBLIC 
    "${PROJECT_BINARY_DIR}"
)

install(TARGETS Tutorial DESTINATION bin)
install(FILES "${PROJECT_BINARY_DIR}/TutorialConfig.h" DESTINATION include)

enable_testing()

add_test(NAME Happy COMMAND Tutorial 25)
add_test(NAME Sad COMMAND Tutorial 42)
add_test(NAME trythis COMMAND ls -la)

