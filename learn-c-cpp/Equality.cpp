/*
 * Unlike Python, the default equality operator is good enough. In
 * Python the default equality will check if the two variables point
 * to the exact same object. In C++ it does a data memeber check.
 */

#include <string>
#include <format>

class Cookie {
private:
    int calories = 0;
    std::string flavor;

public:
    Cookie();

    Cookie(int calories, std::string_view flavor);

    Cookie(const Cookie &copy);
    Cookie& operator=(const Cookie &copy);

    Cookie(Cookie &&move) noexcept;
    Cookie& operator=(Cookie &&move) noexcept;

    bool operator==(const Cookie&) const = default;

    [[nodiscard]] int getCalories() const;
    [[nodiscard]] std::string getFlavor() const;
    void setCalories(int calories);
    void setFlavor(std::string_view flavor);
};

template<>
struct std::formatter<Cookie> : std::formatter<std::string> {
    auto format(const Cookie& cookie, format_context& ctx) const {
        return formatter<string>::format(
            std::format("<Cookie(calories={}, flavor={})>", cookie.getCalories(), cookie.getFlavor()), ctx);

    }
};

Cookie::Cookie() {
    printf("default ctor addr: %p\n", this);
}

Cookie::Cookie(const int calories, const std::string_view flavor) : calories(calories), flavor(flavor) {}

void Cookie::setCalories(const int calories) {
    this->calories = calories;
}

void Cookie::setFlavor(const std::string_view flavor) {
    this->flavor = flavor;
}


Cookie::Cookie(const Cookie &copy) : calories(copy.calories), flavor(copy.flavor) {
    std::cout << "Copy Constructor" << std::endl;
}

Cookie &Cookie::operator=(const Cookie &copy) {
    std::cout << "Copy Assignment" << std::endl;
    if (this != &copy) {
        calories = copy.calories;
        flavor = copy.flavor;
    }
    return *this;
}

Cookie::Cookie(Cookie&& move) noexcept : calories(move.calories), flavor(move.flavor) {
    std::cout << "Move Constructor" << std::endl;
}

Cookie &Cookie::operator=(Cookie &&move) noexcept {
    std::cout << "Move Assignment" << std::endl;
    if (this != &move) {
        calories = move.calories;
        flavor = move.flavor;
    }
    return *this;
}

// bool Cookie::operator==(const Cookie& other) const {
//   if (this == &other) return true;
//   return flavor == other.flavor && calories == other.calories;
// }

int Cookie::getCalories() const {
    return calories;
}

std::string Cookie::getFlavor() const {
    return flavor;
}

#include <iostream>
#include <string>
#include <format>
#include <vector>

#include "cookie.h"
#include <vector>
// #include <algorithm>
// #include <unordered_map>

using namespace std::string_literals;

int main() {
    const auto cookies1 = std::vector {
        Cookie{200, "Chocolate Chip"},
        Cookie{220, "Snicker Doodle"},
        Cookie{180, "Oatmeal Raisin"}
    };

    const auto cookies2 = std::vector {
        Cookie{200, "Chocolate Chip"},
        Cookie{220, "Snicker Doodle"},
        Cookie{180, "Oatmeal Raisin"}
    };

    bool are_equal = cookies1 == cookies2;
    std::cout << std::format("Expected cookie vectors to be equal. Actual are equal: {}", are_equal) << std::endl;

    are_equal = cookies1[0] == cookies2[0];
    std::cout << std::format("Expected the first elements of both vectors to be equal. Actual are equal: {}", are_equal) << std::endl;

    are_equal = cookies1[0] == cookies2[1];
    std::cout << std::format("Expected first element to be not equal to the second element. Actual are equal: {}", are_equal) << std::endl;

}





