#include <iostream>

using namespace std;

// Template with a single type
template <typename T>
T add(T x, T y) {
  // T should support operator+
  return x + y;
}

// Template with multiple types
template <typename T, typename U>
bool are_equal(T x, U y) {
  // It should be possible to cast T and U to int
  return int(x) == int(y);
}

// Template with non-type template params
// This seems like closure, but it is not. N has to be determined at
// compile time. It cannot be a variable.
// An example is the standard array class that has to be declared with its size.
template <typename T, int N>
T fixed_multiply(T val) {
  return val * N;
}

int main() {
  // No need to specify which template type T is being used
  // it is clear from context that I am calling add<int>
  int c = add(10, 20);

  // In the following I have to specify the type
  double z1 = add<double>(10, 20.2);
  double z2 = add<int>(10, 20.2);
  cout << "z1=" << z1 << " z2=" << z2 << endl;

  bool eq1 = are_equal<int, double>(10.3, 10.4);
  bool eq2 = are_equal<double, double>(10.3, 10.4);
  cout << "eq1=" << eq1 << " eq2=" << eq2 << endl;

  double r = fixed_multiply<double, 10>(2.2);
  cout << "r=" << r << endl;
}
