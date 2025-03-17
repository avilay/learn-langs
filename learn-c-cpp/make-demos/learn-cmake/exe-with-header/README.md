```shell
cmake -DCMAKE_BUILD_TYPE=Debug \
      -DCMAKE_C_COMPILER=gcc \
      -DCMAKE_EXPORT_COMPILE_COMMANDS=YES \
      -DCMAKE_C_FLAGS=-Wall \
      -S . -B build
      
cmake --build build

build/exe-with-header
```
