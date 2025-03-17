/*
 * This sample demonstrates that abstract base classes, i.e., classes with pure
 * virtual methods cannot be instantiated. It also demonstrates that all
 * concrete base classes must implement the pure virtual methods. At run time
 * the base pointer will call the implementation of whichever full child object
 * it is pointing to. The third thing this demos is that methods in the base
 * class can also invoke the child class implementation depending on which full
 * object they are parented to.
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

  void printArea() {
    int a = this->area();
    std::cout << "Area=" << std::to_string(a) << std::endl;
  }

  virtual int area() = 0;
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
  int area() {
    std::cout << "Triangle::area" << std::endl;
    return (m_height * m_width) / 2;
  }
};

int main(int argc, char **argv) {
  // Cannot instantiate polygon object
  // Polygon *poly = new Polygon();

  Polygon *poly1 = new Rectangle();
  poly1->setValues(4, 5);
  // Will print Rectangle::area
  poly1->printArea();

  Polygon *poly2 = new Triangle();
  poly2->setValues(4, 5);
  // Will print Triangle::area
  poly2->printArea();
}
