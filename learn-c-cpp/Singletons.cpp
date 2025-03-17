#include <format>
#include <iostream>
#include <memory>
#include <sstream>

unsigned int addrin(void* ptr) {
  std::stringstream ss;
  ss << ptr;
  std::string hexaddr;
  ss >> hexaddr;
  unsigned int addr = stoul(hexaddr, nullptr, 16);
  return addr;
}

// Old style singleton class
class SingletonOne {
private:
  SingletonOne(){};
  static SingletonOne* instance;

public:
  SingletonOne(const SingletonOne&) = delete;
  void operator=(const SingletonOne&) = delete;
  static SingletonOne* getInstance(); // Don't implement this here.
};

SingletonOne* SingletonOne::instance = nullptr;
SingletonOne* SingletonOne::getInstance() {
  if (!instance) {
    instance = new SingletonOne{};
  }
  return instance;
}

// New style singleton class
class SingletonTwo {
private:
  int id;
  SingletonTwo() {}

public:
  static SingletonTwo& getInstance() {
    static SingletonTwo instance{};
    return instance;
  }
  SingletonTwo(const SingletonTwo&) = delete;
  void operator=(const SingletonTwo&) = delete;
};

int main() {
  SingletonOne* one{SingletonOne::getInstance()};
  std::cout << std::format("SingltonOne addr: {}", addrin(one)) << std::endl;

  SingletonOne* dupOne{SingletonOne::getInstance()};
  std::cout << std::format("SingltonOne addr: {}", addrin(dupOne)) << std::endl;

  SingletonTwo& two{SingletonTwo::getInstance()};
  std::cout << std::format("SingltonTwo addr: {}", addrin(&two)) << std::endl;

  SingletonTwo& dupTwo{SingletonTwo::getInstance()};
  std::cout << std::format("SingltonTwo addr: {}", addrin(&dupTwo))
            << std::endl;
}