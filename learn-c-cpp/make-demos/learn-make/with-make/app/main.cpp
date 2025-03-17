/*
 * Install glew and glfw
 * $ brew install glew
 * $ brew install glfw
 *
 * Build this source as follows:
 * clang++ -g main.cpp -std=c++17 -o main -lglfw.3 -lGLEW.2.1 -framework OpenGL
 * -framework Cocoa -framework IOKit
 *
 */

#include <iostream>

#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "utils/Point.h"
#include "utils/Triangle.h"

int main(int argc, char** argv) {
  std::cout << "Starting..." << std::endl;

  GLFWwindow* window;
  if (!glfwInit())
    return -1;

  window = glfwCreateWindow(640, 480, "Hello World", NULL, NULL);
  if (!window) {
    glfwTerminate();
    return -1;
  }

  glfwMakeContextCurrent(window);

  if (glewInit() != GLEW_OK)
    return -1;

  std::cout << "OpenGL ver=" << glGetString(GL_VERSION) << std::endl;

  Triangle triangle = Triangle(0, 0, 0.5);
  std::cout << "Drawing a triangle with vertices -" << std::endl;
  std::cout << "A=(" << triangle.getVertexA().x << ","
            << triangle.getVertexA().y << ")" << std::endl;
  std::cout << "B=(" << triangle.getVertexB().x << ","
            << triangle.getVertexB().y << ")" << std::endl;
  std::cout << "C=(" << triangle.getVertexC().x << ","
            << triangle.getVertexC().y << ")" << std::endl;

  while (!glfwWindowShouldClose(window)) {
    glClear(GL_COLOR_BUFFER_BIT);

    glBegin(GL_TRIANGLES);
    glVertex2f(triangle.getVertexA().x, triangle.getVertexA().y);
    glVertex2f(triangle.getVertexB().x, triangle.getVertexB().y);
    glVertex2f(triangle.getVertexC().x, triangle.getVertexC().y);
    glEnd();

    glfwSwapBuffers(window);
    glfwPollEvents();
  }
}
