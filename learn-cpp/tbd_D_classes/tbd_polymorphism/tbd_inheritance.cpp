/*
 * A `public` inheritance means that all the accessors of the base class are
 * avaialable as-is to users of the derived class. A `protected` inheritance
 * means that all `public` memebers of the base class will be available as
 * `protected` in the derived class, everything else remains as-is. A `private`
 * inheritance means that all `public` and `protected` members of the base class
 * are available as `private` in the derived class. I cannot readily think of
 * scenarios where I'd want to make the inheritance anything other than
 * `public`.
 *
 * By default, when the derived class is created, the default base ctor is
 * called, like in the `Rectangle` class. Of course the child class can specify
 * which base ctor to use, like the `Triangle` class is doing.
 */

#include <iostream>

class Polygon {
protected:
  int m_width;
  int m_height;

public:
  Polygon() { std::cout << "Polygon default ctor" << std::endl; };

  Polygon(int width, int height) : m_width(width), m_height(height) {
    std::cout << "Polygon param ctor" << std::endl;
  }
};

class Rectangle : public Polygon {
public:
  Rectangle(int width, int height) {
    std::cout << "Rectangle param ctor" << std::endl;
    m_width = width;
    m_height = height;
  };

  int area() {
    std::cout << "Rectangle::area" << std::endl;
    return m_width * m_height;
  };
};

class Triangle : public Polygon {
public:
  Triangle(int width, int height) : Polygon(width, height) {
    std::cout << "Triange param ctor" << std::endl;
  }

  int area() {
    std::cout << "Triange::area" << std::endl;
    return (m_width * m_height) / 2;
  };
};

int main(int argc, char **argv) {
  auto rectangle = Rectangle(4, 5);
  int rarea = rectangle.area();

  auto triangle = Triangle(4, 5);
  int tarea = triangle.area();

  std::cout << "Area of rectangle is " << std::to_string(rarea) << std::endl;
  std::cout << "Area of triangle is " << std::to_string(tarea) << std::endl;
}
