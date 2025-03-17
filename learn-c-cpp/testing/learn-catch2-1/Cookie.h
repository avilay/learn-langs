//
// Created by avilay on 12/28/24.
//

#pragma once
#include <string>


class Cookie {
  int calories = 0;
  std::string flavor {};

  Cookie() = default;
  Cookie(int calories, const std::string& flavor);

public:
  static Cookie& getInstance();
  void setCalories(int calories);
  void setFlavor(const std::string& flavor);
  [[nodiscard]] int getCalories() const;
  [[nodiscard]] std::string getFlavor() const;
};
