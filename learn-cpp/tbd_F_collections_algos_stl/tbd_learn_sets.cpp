/*
 * There are two implementations of sets in C++ - `set` and `unordered_set`.
 * I'll default to `unordered_set` because it is implemented as a hash table
 * instead of binary search tree. There two types of sets - ones that only hold
 * unique keys (`unordered_set`) and one that can hold duplicate keys
 * (`unordered_multiset`).
 */

#include <iostream>
#include <unordered_set>

std::ostream &operator<<(std::ostream &out, std::unordered_set<int> set) {
  out << "{ ";
  for (int x : set) {
    out << x << " ";
  }
  out << "}";
  return out;
}

std::ostream &operator<<(std::ostream &out, std::unordered_multiset<int> set) {
  out << "{ ";
  for (int x : set) {
    out << x << " ";
  }
  out << "}";
  return out;
}

int main(int argc, char **argv) {
  std::unordered_set<int> uniqs{3, 4, 1, 3, 2, 3, 5};
  std::cout << "Note no dups: " << uniqs << std::endl;

  std::unordered_multiset<int> dups{3, 4, 1, 3, 2, 3, 5};
  std::cout << "Note presence of dups: " << dups << std::endl;

  // add a new element in the set
  uniqs.insert(6);
  dups.insert(6);
  std::cout << "uniqs after a single insert: " << uniqs << std::endl;
  std::cout << "dups after a single insert: " << dups << std::endl;

  // to check if an element exists check its count
  std::cout << "uniqs.count(10)=" << uniqs.count(10) << std::endl;
  std::cout << "uniqs.count(3)=" << uniqs.count(3) << std::endl;
  std::cout << "dups.count(3)=" << dups.count(3) << std::endl;

  // to actually retrieve an element from the set use find
  auto it = uniqs.find(3);
  if (it == uniqs.end()) {
    std::cout << "3 not found in uniqs" << std::endl;
  } else {
    std::cout << *it << " found in uniqs" << std::endl;
  }
  // but it is not possible to change this value - the following will not
  // compile *it = 50; in the dups set find will get one of the values
  int val = 3;
  it = dups.find(val);
  if (it == dups.end()) {
    std::cout << val << " not found in dups" << std::endl;
  } else {
    std::cout << *it << " found in dups" << std::endl;
  }
  // to get all the values use equal_range
  auto its = dups.equal_range(val);
  for (auto curr = its.first; curr != its.second; curr++) {
    std::cout << *curr << " ";
  }
  std::cout << std::endl;

  // to remove an element use erase - either provide the value directly or use
  // an iter
  uniqs.erase(3);
  std::cout << "After uniqs.erase(3): " << uniqs << std::endl;
  // in dups it will remove all the occurences
  dups.erase(3);
  std::cout << "After dups.erase(3): " << dups << std::endl;
}
