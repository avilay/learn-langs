/*
 * In this sample program, the base class has a virtual function which is
 * invoked if the derived class does not have implementation of this method.
 * Otherwise the derived class implementation is invoked.
 */
#include <iostream>

class Polygon {
protected:
  int m_height;
  int m_width;

public:
  void setValues(int width, int height) {
    m_height = height;
    m_width = width;
  }

  virtual int area() {
    std::cout << "Polygon::area" << std::endl;
    return 0;
  }
};

class Rectangle : public Polygon {
public:
  int area() {
    std::cout << "Rectangle::area" << std::endl;
    return m_height * m_width;
  }
};

class Triangle : public Polygon {
public:
  int getBase() { return m_width; }
};

void printArea(Polygon *poly) {
  int a = poly->area();
  std::cout << "Area=" << std::to_string(a) << std::endl;
}

int main(int argc, char **argv) {
  Rectangle *rect = new Rectangle();
  rect->setValues(4, 5);
  printArea(rect);

  Triangle *tri = new Triangle();
  tri->setValues(4, 5);
  printArea(tri);
}
