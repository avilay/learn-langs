/*
 * This module provides a quick overview of the following:
 *   - Different use cases of the using keyword
 *   - Simple operator overloading
 *   - The most common smart pointer - the shared_ptr
 *   - The most common STL container - vector
 */

#include <iostream>
#include <sstream>
#include <vector>

template <typename T>
std::ostream& operator<<(std::ostream& out, std::vector<T> arr) {
  std::stringstream strbuf{};
  strbuf << "[";
  for (T x : arr) {
    strbuf << x << ", ";
  }
  auto str = strbuf.str();
  str.replace(str.length() - 2, std::string::npos, "]");
  out << str;
  return out;
}

using Tokens = std::vector<std::string>;
using TokensPtr = std::shared_ptr<Tokens>;

TokensPtr split(const std::string& input, const std::string& delim) {
  auto tokens = TokensPtr{new Tokens{}};
  size_t start = 0;

  auto pos = input.find(delim);
  while (pos != std::string::npos) {
    if (start < pos) {
      auto token = input.substr(start, (pos - start));
      tokens->push_back(token);
    }
    start = pos + delim.length();
    pos = input.find(delim, start);
  }
  if (start < input.length()) {
    auto final_token = input.substr(start, (input.length() - start));
    tokens->push_back(final_token);
  }

  return tokens;
}

int main(int argc, char** argv) {
  std::vector<int> arr{1, 2, 3};
  std::cout << arr << std::endl;

  std::string csv = "hello,world";
  std::string delim = ",";
  auto tokens = split(csv, delim);
  std::cout << "Tokens: " << tokens << std::endl;
}
