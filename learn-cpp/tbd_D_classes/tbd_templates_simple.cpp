#include <iostream>
using namespace std;

template <class T> class MyPair {
  T a, b;

public:
  MyPair(T first, T second) : a(first), b(second) {}
  T max();
};

template <class T> T MyPair<T>::max() {
  T retval;
  retval = a > b ? a : b;
  return retval;
}

int main() {
  MyPair<int> pair1{10, 20};
  cout << pair1.max() << endl;

  MyPair<char> pair2{'a', 'b'};
  cout << pair2.max() << endl;
}