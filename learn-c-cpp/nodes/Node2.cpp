#include <format>
#include <iostream>

class Node {
private:
  int val;
  Node* next;

public:
  Node(int val, Node* next) : val(val), next(next) {}
  Node(int val) : val(val), next(nullptr) {}
  Node() : val(int{}), next(nullptr) {}

  int getVal() { return val; }
  Node* getNext() { return next; }
};

void printNode(Node* root) {
  Node* curr{root};
  while (curr) {
    std::cout << std::format("{} ->", curr->getVal());
    curr = curr->getNext();
  }
  std::cout << "||" << std::endl;
}

int main() {
  Node* five{new Node{5}};
  Node* four{new Node{4, five}};
  Node* three{new Node{3, four}};
  Node* two{new Node{2, three}};
  Node* one{new Node{1, two}};
  printNode(one);
}
