//
// Created by avilay on 12/28/24.
//

#include "Cookie.h"

Cookie::Cookie(const int calories, const std::string& flavor) {
  this->calories = calories;
  this->flavor = flavor;
}

Cookie& Cookie::getInstance() {
  static Cookie instance;
  return instance;
}

void Cookie::setCalories(const int calories) {
  this->calories = calories;
}

void Cookie::setFlavor(const std::string& flavor) {
  this->flavor = flavor;
}

int Cookie::getCalories() const {
  return calories;
}

std::string Cookie::getFlavor() const {
  return flavor;
}
