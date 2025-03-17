/*
 * Structs and classes are equivalent. The only difference seems that the
 * default accessor in structs is public whereas in classes it is private.
 */
#include <math.h>
#include <iostream>

using namespace std;

struct Vector2D {
  // Everything is public by default
  double x1_;
  double x2_;

  Vector2D();
  Vector2D(double x1val, double x2val);
  double l2Norm();
  Vector2D& add(const Vector2D&);
};

Vector2D::Vector2D() {
  x1_ = 0;
  x2_ = 0;
}

Vector2D::Vector2D(double x1val, double x2val) {
  x1_ = x1val;
  x2_ = x2val;
}

double Vector2D::l2Norm() {
  return sqrt(pow(x1_, 2) + pow(x2_, 2));
}

Vector2D& Vector2D::add(const Vector2D& other) {
  double newx1 = x1_ + other.x1_;
  double newx2 = x2_ + other.x2_;

  // Vector2D* result = new Vector2D(x1, x2);
  // return *result;
  return *(new Vector2D(newx1, newx2));
}

class Cookie {
  // Everything is private by default
  // Everything can be implemented in the class C# style
  // Called header-only implementation
  int calories_;
  string flavor_;

 public:
  Cookie(int calories, string flavor) {
    calories_ = calories;
    flavor_ = flavor;
  }

  string flavor() { return flavor_; }

  int calories() { return calories_; }
};

int main() {
  auto v1 = Vector2D(1, 1);
  auto v2 = Vector2D(2, 2);
  auto v3 = v1.add(v2);
  cout << v3.x1_ << " " << v3.x2_ << endl;

  auto cc = Cookie(200, "Chocolate Chip");
  cout << cc.calories() << " " << cc.flavor() << endl;
}
