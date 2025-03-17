// clang++ -std=c++20 -g -Wall Main.cpp

#include <charconv>
#include <exception>
#include <format>
#include <fstream>
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cout << "./a.out NUMBER" << std::endl;
    return 1;
  }
  std::string argNum{argv[1]};
  unsigned char num{};
  auto start = argNum.data();
  auto end = argNum.data() + argNum.size();
  auto [ptr, ec] = std::from_chars(start, end, num);
  if (ec != std::errc{}) {
    switch (ec) {
    case std::errc::invalid_argument:
      std::cout << "Invalid argument." << std::endl;
      break;
    case std::errc::result_out_of_range:
      std::cout << "Out of range." << std::endl;
      break;
    default:
      std::cout << "Unknown error." << std::endl;
    }
    return 1;
  }
  std::cout << std::format("No error. Entered: {}", num) << std::endl;
}