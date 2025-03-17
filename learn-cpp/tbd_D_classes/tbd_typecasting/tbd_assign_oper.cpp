#include <iostream>

class Apple {
  int m_id;
  std::string m_variety;

public:
  Apple(const std::string &variety) : m_variety(variety) {
    m_id = rand() % 100;
  }

  std::string &variety() const { return *(new std::string(m_variety)); }
};

class Orange {
  float m_avg_size;
  std::string m_name;

public:
  Orange(const std::string &variety) : m_name(variety) {
    m_avg_size = rand() % 10;
  }

  Orange &operator=(const Apple &apple) {
    std::cout << "Orange conversion operator=" << std::endl;
    m_avg_size = rand() % 10;
    m_name = "From Apple " + apple.variety() + " " + m_name;
    return *this;
  }

  std::string &repr() {
    std::string &ret = *(new std::string("<Orange("));
    ret += "avg size=" + std::to_string(m_avg_size) + " ";
    ret += "name=" + m_name;
    ret += ")>";
    return ret;
  }
};

int main(int argc, char **argv) {
  Apple apple("Honey Crisp");
  Orange orange("Satsuma");
  orange = apple;
  std::cout << orange.repr() << std::endl;

  // The below will not compile
  // Orange orange2 = apple;
}