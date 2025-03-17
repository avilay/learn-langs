#include <iostream>

#include "Point.h"
#include "Triangle.h"

int main(int argc, char** argv) {
  std::cout << "Starting..." << std::endl;

  Triangle triangle = Triangle(0, 0, 0.5);
  std::cout << "Drawing a triangle with vertices -" << std::endl;
  std::cout << "A=(" << triangle.getVertexA().x << ","
            << triangle.getVertexA().y << ")" << std::endl;
  std::cout << "B=(" << triangle.getVertexB().x << ","
            << triangle.getVertexB().y << ")" << std::endl;
  std::cout << "C=(" << triangle.getVertexC().x << ","
            << triangle.getVertexC().y << ")" << std::endl;
}
