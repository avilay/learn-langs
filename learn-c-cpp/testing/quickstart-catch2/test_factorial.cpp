//
// Created by avilay on 12/28/24.
//

#include <catch2/catch_test_macros.hpp>
#include "factorial_impl.h"

TEST_CASE("Factorials are computed", "[factorial]") {
  REQUIRE(factorial(10) == 3628800);
  REQUIRE(factorial(1) == 1);
  REQUIRE(factorial(2) == 2);
  REQUIRE(factorial(3) == 6);
  REQUIRE(factorial(0) == 1);
}
