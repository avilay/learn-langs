/* THERE IS NO WAY I CAN MAKE THIS WORK WITHOUT POINTERS!! */

#include <format>
#include <iostream>
#include <sstream>
#include <typeinfo>

unsigned int addrin(void* ptr) {
  std::stringstream ss;
  ss << ptr;
  std::string hexaddr;
  ss >> hexaddr;
  unsigned int addr = stoul(hexaddr, nullptr, 16);
  return addr;
}

// class Node {
// public:
//   Node() = default;
//   Node(const Node&) = delete;
//   Node(Node&&) = delete;
//   Node& operator=(const Node&) {

//   }
//   Node& operator=(Node&&) = delete;

//   virtual std::string toString() const { return "Node"; }
//   virtual int myAddr() { return addrin(this); }
// };

// class ListNode : public Node {

class ListNode {
private:
  int val;
  ListNode& next;

public:
  ListNode(int val, ListNode& next) : val(val), next(next) {}
  ListNode(int val) : ListNode(val, getSingleton()) {}
  ListNode() : ListNode(0, getSingleton()) {}
  ListNode(const ListNode&) = delete;
  ListNode(ListNode&&) = delete;

  ListNode& operator=(const ListNode& that) {
    if (this == &that) {
      return *this;
    }
    val = that.val;
    next = that.next;
    return *this;
  }
  // ListNode& operator=(const ListNode&) = delete;

  ListNode& operator=(ListNode&&) = delete;

  int getVal() { return val; }

  ListNode& getNext() {
    std::cout << std::format("ListNode::getNext returning next at addr {}",
                             addrin(this))
              << std::endl;
    return next;
  }

  virtual std::string toString() const {
    return std::format("ListNode({})", val);
  }
};

class EmptyNode : public ListNode {
public:
  std::string toString() const override { return "EmptyNode"; }
};

// EmptyNode& getInstance() {
//   static EmptyNode instance{};
//   return instance;
// }

void printNode(ListNode& curr) {
  std::cout << "\nInside printNode" << std::endl;

  std::cout << curr.toString() << std::endl;
  curr = curr.getNext();
}

int main() {
  ListNode five{5};
  std::cout << five.getNext().toString() << std::endl;

  ListNode four{4, five};
  std::cout << four.getNext().toString() << std::endl;

  ListNode three{3, four};
  std::cout << three.getNext().toString() << std::endl;

  ListNode two{2, three};
  std::cout << two.getNext().toString() << std::endl;

  ListNode one{1, two};
  std::cout << one.getNext().toString() << std::endl;

  printNode(one);
}