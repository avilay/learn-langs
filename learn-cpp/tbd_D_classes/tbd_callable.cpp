/*
 * Creating a callable can be done with the simple trick of overloading the
 * operator(). There are some "function objects" which are simply callables. One
 * such built-in callable is the hash class.
 */

#include <iostream>

struct MyCallable {
  size_t operator()(const std::string &input) { return input.length(); }
};

int main(int argc, char **argv) {
  MyCallable callable{};
  std::cout << callable("hello") << std::endl;

  std::hash<std::string> hash_fn{};
  std::cout << hash_fn("hello") << std::endl;
}
