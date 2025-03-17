#include <format>
#include <iostream>

#include "factorial_impl.h"

int main() {
  unsigned int fact_10 = factorial(10);
  std::cout << std::format("10! = {}", fact_10) << std::endl;
  return 0;
}
