#include <iostream>
using namespace std;

int gcd(int a, int b) {
  int tmp = 0;
  while (b != 0) {
    tmp = b;
    b = a % b;
    a = tmp;
  }
  return a;
}

int gcd2(int a, int b) {
  if (b == 0) {
    return a;
  } else {
    return gcd2(b, a % b);
  }
}

int main() {
  int a = 0;
  int b = 0;
  cout << "Enter first number: ";
  cin >> a;
  cout << "Enter second number: ";
  cin >> b;
  cout << "GCD of " << a << " and " << b << " is " << gcd2(252, 105) << endl;
  return 0;
}
