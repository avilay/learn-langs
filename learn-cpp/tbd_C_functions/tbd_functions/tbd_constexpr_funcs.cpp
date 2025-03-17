#include <iostream>

// `constexpr` tells the compiler that the value returned by this
// function can be determined at compile time and as such can be
// used anywhere compile time constant values are needed
constexpr int product(int x, int y) {
  return x * y;
}

int main() {
  const int x = 2;
  const int y = 3;
  const int len = product(x, y);
  int arr[len]{};
  for (auto x : arr) {
    std::cout << x << " ";
  }
  std::cout << std::endl;
}