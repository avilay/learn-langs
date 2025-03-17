cmake_minimum_required(VERSION 3.17)

# The usual, set a bunch of project related variables. For details look at Step1.
project(Tutorial VERSION 1.0)

# Set some other variables that are used by the cmake system. For details look at Step2.
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# An option is something that can be set when running cmake for different build behavior.
# As can be seen below it is used inside an if.
# In TutorialCOnfig.h.in it can also be translated as an #define if the option is set to ON.
option(USE_MYMATH "Use tutorial provided math implementation" ON)

# See Step1.
configure_file(TutorialConfig.h.in TutorialConfig.h)

# First use of the option, for a second use see TutorialConfig.h.in
if (USE_MYMATH)
	# First do the usual, go the MathFunctions directory and do what the CMakeLists.txt tells you to do.
	add_subdirectory(MathFunctions)

	# Set a list variable called EXTRA_LIBS and append AptgMathFunctions to it.
	list(APPEND EXTRA_LIBS AptgMathFunctions)

	# Set a list variable called EXTRA_INCLUDES and append ./MathFunctions to it.
	list(APPEND EXTRA_INCLUDES "${PROJECT_SOURCE_DIR}/MathFunctions")
endif()

# Same as Step1
add_executable(Tutorial tutorial.cxx)

# Almost same as Step1, execept use the EXTRA_LIBS, EXTRA_INCLUDES lists. If they are empty no harm done.
target_link_libraries(Tutorial PUBLIC ${EXTRA_LIBS})
target_include_directories(
	Tutorial PUBLIC 
    "${PROJECT_BINARY_DIR}"
	${EXTRA_INCLUDES}
)
