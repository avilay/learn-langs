cmake_minimum_required(VERSION 3.17)

# Creates a bunch of project specific variables like -
#  - PROJECT_SOURCE_DIR
#  - <PROJECT-NAME>_SOURCE_DIR
#  - PROJECT_BINARY_DIR
#  - <PROJECT-NAME>_BINARY_DIR
#  - PROJECT_VERSION
#  - <PROJECT-NAME>_VERSION
#  - PROJECT_VERSION_MAJOR
#  - <PROJECT-NAME>_VERSION_MAJOR
#  - PROJECT_VERSION_MINOR
#  - <PROJECT-NAME>_VERSION_MINOR
#  - PROJECT_VERSION_PATCH
#  - <PROJECT-NAME>_VERSION_PATCH
#  - PROJECT_VERSION_TWEAK
#  - <PROJECT-NAME>_VERSION_TWEAK.
project(Tutorial VERSION 1.0)

# Just setting some some variables that are used by the cmake system
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# String replace all the variables defined in .h.in file and output the .h file
configure_file(TutorialConfig.h.in TutorialConfig.h)

# Now the real action starts

# Go to this sub-directory and follow the CMakeLists.txt.
# In this particular case it will define a SHARED target called AptgMathFunctions
add_subdirectory(MathFunctions)

# Build this executable with the source files mentioned here
add_executable(Tutorial tutorial.cxx)

# Link the target with the items. In this case the target Tutorial must've been defined
# above as an executable. And AptgMathFunctions must've been a target for add_library.
target_link_libraries(Tutorial PUBLIC AptgMathFunctions)

# Tell cmake where to find the various header files.
# The TutorialConfig.h will end up in the build directory, so including PROJECT_BINARY_DIR,
# The MathFunctions/MathFunctions.h is not copied by the cmake system and will remain in
# ./MathFunctions/MathFunctions.h so the PROJECT_SOURCE_DIR/MathFunctions must be included.
target_include_directories(
	Tutorial PUBLIC 
    "${PROJECT_BINARY_DIR}"
	"${PROJECT_SOURCE_DIR}/MathFunctions"
)
