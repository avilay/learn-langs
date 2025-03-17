#include "Triangle.h"
#include "Point.h"

Triangle::Triangle(float centerX, float centerY, float distance) {
  a = Point(centerX - distance, centerY - distance);
  b = Point(centerX + distance, centerY - distance);
  c = Point(centerX, centerY + distance);
}

Point Triangle::getVertexA() {
  return a;
}

Point Triangle::getVertexB() {
  return b;
}

Point Triangle::getVertexC() {
  return c;
}
