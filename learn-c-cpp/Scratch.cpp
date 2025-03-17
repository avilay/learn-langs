#include <format>
#include <iostream>
#include <optional>
#include <queue>

class Cookie {
private:
  int calories;
  std::string flavor;

public:
  Cookie(int calories, const std::string& flavor)
      : calories(calories), flavor(flavor) {}

  std::string toString() {
    return std::format("<Cookie(calories={}, flavor={})>", calories, flavor);
  }
};

void exp1() {
  std::queue<int> intq{};
  intq.push(1);
  intq.push(2);
  while (not intq.empty()) {
    int& x = intq.front();
    intq.pop();
    std::cout << x << std::endl;
  }
}

int main() {
  std::shared_ptr<Cookie> cptr1{
      std::make_shared<Cookie>(200, "Chocolate Chip")};
  std::shared_ptr<Cookie> cptr2{
      std::make_shared<Cookie>(180, "Snicker Doodle")};

  std::queue<std::shared_ptr<Cookie>> cookieq{};
  cookieq.push(cptr1);
  cookieq.push(cptr2);
  while (not cookieq.empty()) {
    auto cookie = cookieq.front();
    cookieq.pop();
    std::cout << cookie->toString() << std::endl;
  }
}