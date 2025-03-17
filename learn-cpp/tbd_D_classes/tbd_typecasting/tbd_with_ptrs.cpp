#include <iostream>

class Apple {
  int m_crispiness;
  std::string m_variety;

public:
  Apple(int crispiness, const std::string &variety)
      : m_crispiness(crispiness), m_variety(variety) {}
  int crispiness() const { return m_crispiness; }
  std::string variety() const { return m_variety; }
};

class Orange {
  float m_avg_size;
  std::string m_name;

public:
  Orange() : m_avg_size(5), m_name("Satsuma") {}
  std::string repr() {
    std::string ret("<Orange(");
    ret += "avg size=" + std::to_string(m_avg_size) + " ";
    ret += "name=" + m_name;
    ret += ")>";
    return ret;
  }
};

int main(int argc, char **argv) {
  Apple *apple = new Apple(100, "Honey Crisp");
  // with an explicit cast, any pointer can be typecast to any other type!
  Orange *orange = (Orange *)apple;
  std::cout << orange->repr() << std::endl;

  // any pointer can be implicitly typecast to void*
  void *ptr = apple;

  // but casting it back needs to be explicit
  Apple *apl = (Apple *)ptr;
}
