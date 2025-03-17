/*
My preferred style is based on LLVM with the following modifications -
  PointerAlignment: Left
  AccessModifierOffset: -1

To see what the full LLVM format looks like run -
$ clang-format -style=llvm -dump-config

To see what the different attributes mean and their allowed values see -
https://clang.llvm.org/docs/ClangFormatStyleOptions.html
*/

#include <iostream>

namespace style_demo {

const int kLogLevel = 1;

enum class Color { DarkRed, LightGreen, NavyBlue };

class CookieJar {
 private:
  std::string cookieFlavor_;

  int cookieCalories_;

 public:
  CookieJar(const std::string& flavor, int calories)
      : cookieFlavor_(flavor), cookieCalories_(calories) {}

  std::string getFlavor() { return cookieFlavor_; }

  int getCalories() { return cookieCalories_; }
};

int eatCookie() {
  int numCookies{10};
  float servingSize = (2 * numCookies + 10) / 3.0;

  if (servingSize > 23.4) {
    std::cout << "Woah! That is a lot of cookies!" << std::endl;
  }

  for (int i = 0; i < numCookies; i++) {
    std::cout << "Eating cookie!" << std::endl;
  }
  CookieJar* jar = new CookieJar("Chocolate Chip", 200);
  std::cout << jar->getCalories() << std::endl;

  CookieJar fixedJar{"Snicker Doodle", 220};
  std::cout << fixedJar.getFlavor() << std::endl;

  return static_cast<int>(servingSize);
}

template <typename T> T add(T x, T y) { return x + y; }

template <class MyClass> class MyPair {
 private:
  T a;
  T b;

 public:
  MyPair(T first, T second) : a(first), b(second) {}
  T max() { return a > b ? a : b; }
};

class ClassThatHasAVeryLongNameToDemoIdentation {};
class AnotherClass {
 public:
  ClassThatHasAVeryLongNameToDemoIdentation
  methodThatAlsoHasAVeryLongNameToDemoIndentation(const CookieJar);
};

ClassThatHasAVeryLongNameToDemoIdentation
AnotherClass::methodThatAlsoHasAVeryLongNameToDemoIndentation(
    const CookieJar jar) {
  std::cout << "Doing something useful" << std::endl;
  std::cout << "Continuing to do something useful" << std::endl;
}

} // namespace style_demo