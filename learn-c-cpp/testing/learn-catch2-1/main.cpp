#include <iostream>

#include "Cookie.h"

int main() {
  Cookie& myCookie = Cookie::getInstance();
  myCookie.setCalories(200);
  myCookie.setFlavor("Choclate Chip");
  std::cout << myCookie.getCalories() << std::endl;
}
