Build command -
```shell
mkdir bin
clang++ -std=c++17 -g app/main.cpp utils/Point.cpp utils/Triangle.cpp -o bin/opengldemo -L/opt/homebrew/Cellar/glew/2.2.0_1/lib -L/opt/homebrew/Cellar/glfw/3.3.3/lib -lglfw -lGLEW -framework OpenGL -framework Cocoa -framework IOKit -I. -I/opt/homebrew/Cellar/glew/2.2.0_1/include -I/opt/homebrew/Cellar/glfw/3.3.3/include
```
