#include <iostream>

using namespace std;

class Circle {
  double m_radius;

public:
  // The following ctor will first initialize m_radius to its default value and
  // then in the body of the ctor it gets reassigned to the user param.
  Circle(double r) { m_radius = r; }
  // This is not a big deal for primitive types. But for members that are
  // objects this can become a problem. See Cylinder for example. To avoid this
  // problem use the following form: Circule(double r) : m_radius(r) {}

  double area() { return 3.1415 * m_radius * m_radius; }
};

class Cylinder {
  double m_height;
  Circle m_base;

public:
  Cylinder(double radius, double height);
  double volume();
};

// The following will not compile because it will try to first call the default
// ctor for Circle and then inside this ctor body is m_base set properly.
// However, Circle does not have a default ctor, so the call will not compile.
// Cylinder::Cylinder(double radius, double height) {
//   m_height = height;
//   m_base = Circle(radius);
// }
// To avoid this error use the following form. Here m_base(arg) will
// automatically call the ctor of the type of m_base with the arg. Weird - ugh!
Cylinder::Cylinder(double radius, double height)
    : m_base(radius), m_height(height) {}

double Cylinder::volume() { return m_base.area() * m_height; }

int main() {
  // The old-school way of invoking ctors
  Circle c1(10.0);

  // Supposedly the modern way of invoking ctors
  Circle c3{30.0};

  // the following ctor invokations are interesting, they are only possible
  // because a single param ctor has been defined. These calls are implicitly
  // converted to Circle c2{20.0} and Circle c4{40.0}
  // to prevent this behavior mark the ctor with the explicity keyword.
  Circle c2 = 20.0;
  Circle c4 = {40.0};
  cout << "Area of circle is " << c1.area() << endl;

  Cylinder d(10.0, 20.0);
  cout << "Volume of cylinder is " << d.volume() << endl;

  Cylinder d2{10.0, 20.0};
  cout << "Volume of cylinde is " << d2.volume() << endl;

  // And of course I can allocate this object on the heap and get a pointer to
  // it
  Circle *c5 = new Circle(5.0);
  cout << "Area of smaller circle is " << c5->area() << endl;

  // And use the curly braces for this as well.
  Circle *c6 = new Circle{5.0};
  cout << "Area of smaller circle is " << c6->area() << endl;
}
