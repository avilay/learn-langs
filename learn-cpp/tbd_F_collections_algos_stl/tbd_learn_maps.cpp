/*
 * There are two implementations of maps in C++ - `map` and `unordered_map`.
 * I'll default to `unordered_map` because it is implemented as a hash table
 * instead of binary search tree. There two types of maps - ones that only hold
 * unique keys (`unordered_map`) and one that can hold duplicate keys
 * (`unordered_multimap`).
 *
 * In C++ maps can function like defaultdicts and normal dict - use hsh[key]
 * gives defaultdict behavior, i.e, inserts a default value if it is not already
 * there, but hsh.at(key) gives normal dict behavior, i.e., some sort of key not
 * found error.
 */
#include <iostream>
#include <unordered_map>

std::ostream &operator<<(std::ostream &out,
                         std::unordered_map<std::string, float> hsh) {
  out << "{ ";
  for (auto key_val : hsh) {
    std::string key = key_val.first;
    float val = key_val.second;
    out << key << " => " << val << ", ";
  }
  out << "}";
  return out;
}

int main(int argc, char **argv) {
  std::unordered_map<std::string, float> hsh{
      {"pi", 3.141}, {"euler", 2.718}, {"planck", 6.626}};
  std::cout << hsh << std::endl;

  hsh.insert({"avogadro", 6.022});
  std::cout << "After insert: " << hsh << std::endl;

  // Access key-value
  int val = hsh["pi"];
  std::cout << "Value of pi: " << val << std::endl;
  // will insert key gravity in the hsh
  val = hsh["gravity"];
  std::cout << "Value of gravity: " << val << std::endl;
  // will throw
  try {
    val = hsh.at("c");
    std::cout << "Value of c: " << val << std::endl;
  } catch (std::out_of_range oor) {
    std::cout << "Got exception when looking for key c: " << oor.what()
              << std::endl;
  }

  // Check for presence of key by calling hsh.count(key)
  std::cout << "hsh.count(pi)=" << hsh.count("pi") << std::endl;

  // Remove key
  hsh.erase("pi");
  std::cout << "After hsh.erase(pi): " << hsh << std::endl;
}