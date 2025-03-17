#include <iostream>

class Orange;

class Apple {
  int m_id;
  std::string m_variety;

public:
  Apple(const std::string &variety) : m_variety(variety) {
    m_id = rand() % 100;
  }

  explicit operator Orange();
};

class Orange {
  float m_avg_size;
  std::string m_name;

public:
  Orange(const std::string &variety) : m_name(variety) {
    m_avg_size = rand() % 10;
  }

  std::string &repr() {
    std::string &ret = *(new std::string("<Orange("));
    ret += "avg size=" + std::to_string(m_avg_size) + " ";
    ret += "name=" + m_name;
    ret += ")>";
    return ret;
  }
};

Apple::operator Orange() {
  std::cout << "Apple operator()" << std::endl;
  std::string name = "From Apple " + m_variety;
  return Orange(name);
}

int main(int argc, char **argv) {
  Apple apple("Honey Crisp");
  Orange orange("Stasuma");
  std::cout << orange.repr() << std::endl;

  orange = (Orange)apple;
  std::cout << orange.repr() << std::endl;

  Orange orange2("Mandarin");
  orange2 = Orange(apple);
  std::cout << orange2.repr() << std::endl;

  // The below two calls are implicitly converted to Orange(apple)
  // to prevent this behavior mark the operator override as explicit

  // Orange orange3("Cutie");
  // orange3 = apple;
  // std::cout << orange3.repr() << std::endl;

  // Orange orange4 = apple;
  // std::cout << orange4.repr() << std::endl;
}
