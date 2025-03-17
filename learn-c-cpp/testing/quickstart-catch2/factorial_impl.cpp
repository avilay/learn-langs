//
// Created by avilay on 12/28/24.
//

#include "factorial_impl.h"

unsigned int factorial(const unsigned int n) {
  return n < 1 ? 1 : factorial(n - 1) * n;
}
