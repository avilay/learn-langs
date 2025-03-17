#pragma once

#include "Point.h"

class Triangle {
  Point a;
  Point b;
  Point c;

 public:
  Triangle(float centerX, float centerY, float distance);
  Point getVertexA();
  Point getVertexB();
  Point getVertexC();
};
