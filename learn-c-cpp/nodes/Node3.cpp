#include <format>
#include <iostream>
#include <memory>

class Node {
private:
  int val;
  std::shared_ptr<Node> next;

public:
  Node(int val, std::shared_ptr<Node> next) : val(val), next(next) {}

  Node(int val) : Node{val, nullptr} {}

  Node() : Node(int{}, nullptr) {}

  // Really nast ctor, will make a copy of the next node.
  Node(int val, const Node& next) : val(val), next(nullptr) {
    this->next = std::shared_ptr<Node>{new Node{}};
    this->next->val = next.val;
    this->next->next = next.next;
  }

  Node(const Node& copy) = default;

  int getValue() { return val; }

  std::shared_ptr<Node> getNext() { return next; }
};

void nastyPrintNode(Node& root) {
  std::shared_ptr<Node> curr{std::make_shared<Node>(root)};
  while (curr) {
    std::cout << std::format("{} -> ", curr->getValue());
    curr = curr->getNext();
  }
  std::cout << "||" << std::endl;
}

void printNode(std::shared_ptr<Node> root) {
  auto curr{root};
  while (curr) {
    std::cout << std::format("{} -> ", curr->getValue());
    curr = curr->getNext();
  }
  std::cout << "||" << std::endl;
}

void nastyWay() {
  Node five{5};
  Node four{4, five};
  Node three{3, four};
  Node two{2, three};
  Node one{1, two};
  nastyPrintNode(one);
}

int main() {
  auto five{std::make_shared<Node>(5)};
  auto four{std::make_shared<Node>(4, five)};
  auto three{std::make_shared<Node>(3, four)};
  auto two{std::make_shared<Node>(2, three)};
  auto one{std::make_shared<Node>(1, two)};
  printNode(one);
}
