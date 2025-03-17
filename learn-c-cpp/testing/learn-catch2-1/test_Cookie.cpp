//
// Created by avilay on 12/28/24.
//
#include <catch2/catch_test_macros.hpp>
#include "Cookie.h"

TEST_CASE("Getters and setters work", "[cookie]") {
  Cookie& cookie = Cookie::getInstance();
  cookie.setCalories(200);
  cookie.setFlavor("Chocolate Chip");

  REQUIRE(cookie.getCalories() == 200);
  REQUIRE(cookie.getFlavor() == "Chocolate Chip");
}

TEST_CASE("Singleton pattern works", "[cookie]") {
  Cookie& cookie1 = Cookie::getInstance();
  Cookie& cookie2 = Cookie::getInstance();
  cookie1.setCalories(200);
  cookie1.setFlavor("Chocolate Chip");

  REQUIRE(cookie2.getCalories() == 200);
  REQUIRE(cookie2.getFlavor() == "Chocolate Chip");
  REQUIRE(&cookie1 == &cookie2);
}