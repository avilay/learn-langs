/*
 * Lots of good info here -
 * https://en.cppreference.com/w/cpp/string/basic_string Initialize and assign
 * Access
 * Size
 * Insert and concatenate
 * Comparison
 * Removal
 * Search
 */

#include <iostream>
#include <string>

int main() {
  // Initialize and assign
  // Using the uniform initializer here, but really all sorts of different ctors
  // can be used
  std::string greeting{"Hello"};
  // The operator<< is also overloaded
  std::cout << greeting << std::endl;
  greeting = "Namaste";
  std::cout << greeting << std::endl;
  std::cout << "The length of this string is " << greeting.length()
            << std::endl;

  // Both read and write access via the index operator
  char ch = greeting[3];
  std::cout << ch << std::endl;
  greeting[0] = 'n';
  std::cout << greeting << std::endl;

  // Concatenate works as expected
  std::string name = "Sansaar";
  std::string fullGreeting = greeting + " " + name;
  std::cout << fullGreeting << std::endl;

  fullGreeting += "!";
  std::cout << fullGreeting << std::endl;

  // Comparison works as expected
  std::string msg = "namaste";
  if (msg == greeting) {
    std::cout << "works!" << std::endl;
  } else {
    throw std::runtime_error("KA-BOOM!");
  }

  // Removal works mostly as expected
  greeting.clear();  // Clears the entire string
  std::cout << greeting << std::endl;
  greeting = "01234567";  // Lets get it back

  // Contrary to Python, [1, 3] is inclusive range
  // The following will delete 1, 2, 3
  greeting.erase(1, 3);
  std::cout << greeting << std::endl;

  // Finding returns the index of the string where it found the given substrring
  // or a special constant std::string::npos if it did not find the substring.
  auto pos = greeting.find("56");
  if (pos != std::string::npos) {
    std::cout << "Found the substring at position " << pos << std::endl;
  }

  // To string from the input, use the getline function.
  // cin by itself stops reading when it encounters the first space
  greeting = "Hello";
  std::getline(std::cin, name);
  std::cout << greeting << " " << name << std::endl;
}