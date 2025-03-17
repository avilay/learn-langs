/*
 * Very similar to deques in Python. Except these deques allow random access by
 * index. Other iterator based APIs are similar to vectors. APIs to pop the
 * front/back don't work as expected. It simply removes the element at the front
 * or back without returning it. In order to access the element, peek at the
 * front or back first using queue.front() or queue.back(). Then pop the
 * element.
 *
 */

#include <deque>
#include <iostream>

std::ostream &operator<<(std::ostream &out, std::deque<int> queue) {
  out << "[ ";
  for (int elem : queue) {
    out << elem << " ";
  }
  out << "]";
  return out;
}

int main(int argc, char **argv) {
  std::deque<int> q{3, 1, 2, 4};
  std::cout << "Insertion order is maintaiend: " << q << std::endl;

  // Typical queue usage
  while (!q.empty()) {
    int &elem = q.front();
    q.pop_front();
    std::cout << "Popped " << elem << " from front" << std::endl;
    if (elem < 3) {
      q.push_back((rand() % 10) + 3);
    }
  }
}
