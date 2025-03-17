/*
 * Unlike Python, I can pass anything by value or by reference. In contrast, in
 * Python only array or other objects could be passed as reference. The "names"
 * were always by value.
 */
#include <math.h>
#include <iostream>
#include <vector>

using namespace std;

int add(int a, int b) {
  // passing by value - there is no way to change the values of a and b in the
  // calling stack
  a = a * 2;
  b = b * 2;
  cout << "add: a=" << a << " b=" << b << endl;
  return a + b;
}

int addByRef(int& a, int& b) {
  // passing by reference - this function can change the values of a and b in
  // the calling stack.
  a = a * 2;
  b = b * 2;
  cout << "addByRef: a=" << a << " b=" << b << endl;
  return a + b;
}

int addByRefSafe(const int& a, const int& b) {
  // even though the arguments are passed by reference, they are immutable
  // because of const the following two lines will not compile
  // a = a * 2;
  // b = b * 2;
  cout << "addByRefSafe: a=" << a << " b=" << b << endl;
  return a + b;
}

double norm(const vector<int>& vec, int p = 2) {
  // p has a default value of 2. Any other arguments coming after it must
  // also have a default value, similar to Python
  double tot = 0;
  for (auto x : vec) {
    tot += pow(abs(x), p);
  }
  return pow(tot, 1 / double(p));
}

int main() {
  int a = 10;
  int b = 20;

  cout << "Calling by-value add" << endl;
  cout << "main: a=" << a << " b=" << b << endl;
  int c = add(a, b);
  cout << "main after add: a=" << a << " b=" << b << " c=" << c << endl;

  cout << "\nCalling by-ref add" << endl;
  cout << "main: a=" << a << " b=" << b << endl;
  c = addByRef(a, b);
  cout << "main after add: a=" << a << " b=" << b << " c=" << c << endl;

  cout << "\nCalling by-ref with const add" << endl;
  cout << "main: a=" << a << " b=" << b << endl;
  c = addByRefSafe(a, b);
  cout << "main after add: a=" << a << " b=" << b << " c=" << c << endl;

  vector<int> v{1, 2};
  cout << "The (default) L2 norm of the vector is " << norm(v) << endl;
  cout << "The L1 norm of the vector is " << norm(v, 1) << endl;
}
