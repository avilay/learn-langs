cmake_minimum_required(VERSION 3.30)
project(exe-with-dep-lib C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(${PROJECT_NAME} main.c)

# If blah.h is in a well-known include path /usr/include then
# target_include_directories command is not needed.
# target_include_directories(${PROJECT_NAME} PUBLIC /usr/local/include/simple)
target_include_directories(${PROJECT_NAME} PUBLIC /home/avilay/projects/bitbucket/learn/learn-lang/learn-c-cpp/make-demos/learn-cmake/simple-lib/include)

# if libsimple.so is in a "well known" library path like /usr/lib then
# target_link_directories command is not needed.
# target_link_directories(${PROJECT_NAME} PUBLIC /usr/local/lib/)
target_link_directories(${PROJECT_NAME} PUBLIC /home/avilay/projects/bitbucket/learn/learn-lang/learn-c-cpp/make-demos/learn-cmake/simple-lib/web-build)

target_link_libraries(${PROJECT_NAME} PRIVATE simple)
