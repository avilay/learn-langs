#include <iostream>
using namespace std;

int a = 18;
int b = 6;

int function1(int a, int b) { return a - b; }

int function2() {
  int c;
  c = a + b;
  return c;
}

int main() {
  int b = 12;
  int c = 0;
  a = function1(b, a); // a = function1(12, 18) = -6
  c = function2();     // c = 0
  cout << "a: " << a << " b: " << b << " c: " << c << endl;
}