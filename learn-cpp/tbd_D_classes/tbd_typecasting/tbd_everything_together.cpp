#include <iostream>

class Orange;

class Apple {
  int m_id;
  std::string m_variety;

public:
  Apple(const std::string &variety) : m_variety(variety) {
    m_id = rand() % 100;
  }

  std::string variety() const { return m_variety; }

  int crispiness() const { return m_id; }

  explicit operator Orange();
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

Apple::operator Orange() {
  std::cout << "Apple operator()" << std::endl;
  std::string name = "From Apple " + m_variety;
  return Orange(name);
}

int main(int argc, char **argv) {
  Apple apple{"Honey Crisp"};

  // conversion ctor
  Orange orange1{apple};
  std::cout << orange1.repr() << std::endl;

  // implicit single param ctor with assignment will not work
  // Orange orange = apple;

  // Assignment operator
  Orange orange2{"Satsuma"};
  orange2 = apple;
  std::cout << orange2.repr() << std::endl;

  // Typecast operator
  Orange orange3{"Mandarin"};
  orange3 = (Orange)apple;
  std::cout << orange3.repr() << std::endl;
}