#include <iostream>

class Apple {
  int m_id;
  std::string m_variety;

public:
  Apple(const std::string &variety) : m_variety(variety) {
    m_id = rand() % 100;
  }

  std::string variety() const { return m_variety; }

  int crispiness() const { return m_id; }
};

class Orange {
  float m_avg_size;
  std::string m_name;

public:
  Orange(const std::string &variety) : m_name(variety) {
    m_avg_size = rand() % 10;
  }

  explicit Orange(const Apple &apple) {
    std::cout << "Orange conversion ctor" << std::endl;
    m_avg_size = apple.crispiness() / 10.0f;
    m_name = "From Apple " + apple.variety();
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

  // Standard ctor usage
  Orange orange1{apple};
  std::cout << orange1.repr() << std::endl;

  // implicit single param ctor usage
  // Orange orange2 = apple;
  // std::cout << orange2.repr() << std::endl;

  // implicit ctor usage in absence of assignment and the typecast operator
  // the following will not compile if the conversion ctor is marked as explicit

  /*
  Orange orange3{"Satsuma"};
  orange3 = apple;
  std::cout << orange3.repr() << std::endl;

  Orange orange4{"Mandarin"};
  orange4 = (Orange)apple;
  std::cout << orange4.repr() << std::endl;
  Orange orange5{"Cutie"};
  orange5 = Orange(apple);
  std::cout << orange5.repr() << std::endl;
  */
}
