/*
 * Vectors are most similar to Python lists. Some common operations that I do on
 * Python lists are-
 *   * Dynamic sizing and resizing: lst = list() or lst = []
 *   * Appending: lst.append(val)
 *   * Random read access: val = lst[x]
 *   * Random updates: lst[x] = newval
 *   * Finding elements: val in lst or val not in lst
 *   * Slicing: lst[start:end:skip]
 *   * Query the length: len(lst)
 *   * Query index of the first occurance: lst.index(val)
 *   * Remove element: lst.remove(val)
 *   * Remove element(s) at index: del lst[start:end:skip]
 */

#include <iostream>
#include <vector>

std::ostream& operator<<(std::ostream& out, std::vector<int> arr) {
  out << "[ ";
  for (auto x : arr) {
    out << x << " ";
  }
  out << "]";
  return out;
}

void withIters() {
  std::vector<int> arr{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

  // Points to the first element in the sequence
  auto it = arr.begin();
  // std::cout << *it << std::endl;

  // Points to the position after the last element in the sequence
  // derefrencing it leads to undefined behavior
  it = arr.end();
  // std::cout << *it << std::endl;

  // Typical iteration
  it = arr.begin();
  while (it != arr.end()) {
    std::cout << *it << " ";
    it += 1;
  }
  std::cout << std::endl;

  // Can also start at any position and end at any position
  it = arr.begin() + 2;  // at index [2]
  auto end = it + 3;     // at index [5]
  // Prints elements arr[2], arr[3], arr[4]
  while (it != end) {
    std::cout << *it << " ";
    it += 1;
  }
  std::cout << std::endl;

  // Can also be used to modify elements
  it = arr.begin() + 2;
  *it = 200;
  std::cout << arr << std::endl;
}

void pythonCompat() {
  // Dynmaic sizing: Initial declaration creates a 0-sized vector
  std::vector<int> arr;
  std::cout << "Initialized empty array: " << arr << std::endl;

  // Append: equivalent to lst.append(val)
  arr.push_back(0);
  arr.push_back(1);
  arr.push_back(2);
  for (int x : {3, 4, 5, 6, 7}) {
    arr.push_back(x);
  }
  std::cout << "After a few appends (push_back)s: " << arr << std::endl;

  // Random access: works as expected - mostly
  std::cout << "arr[2]=" << arr[2] << std::endl;
  // this does not throw an exception!
  std::cout << "Bad element access: arr[100]=" << arr[100] << std::endl;
  // to get Python-like behavior use at
  std::cout << "arr.at(2)=" << arr.at(2) << std::endl;
  try {
    std::cout << arr.at(100) << std::endl;
  } catch (std::out_of_range oor) {
    std::cout << "Got exception when trying to call arr.at(100): " << oor.what()
              << std::endl;
  }

  // Random updates: works as expected - mostly
  arr[2] = 120;
  std::cout << "After updating arr[2]=" << arr[2] << std::endl;
  // Again this does not throw an exception reliably - sometimes fails silently,
  // other times throws malloc error!
  // arr[10] = 130;

  // No built-in for finding elements - need to traverse the entire vector

  // No slicing - need to use iterators

  // Query the length
  std::cout << "arr.size()=" << arr.size() << std::endl;

  // No way to query the index of a value - need to traverse the vector

  // Removing elements
  // Remove everything
  arr.clear();
  std::cout << "After clearing: " << arr << std::endl;
  // Lets add back the elments for more experimentation
  for (int i : {0, 1, 2, 3, 4, 5, 6, 7}) {
    arr.push_back(i);
  }
  auto it = arr.begin() + 3;  // points to arr[3]
  // Remove single element
  arr.erase(it);
  std::cout << "After erasing arr[3]: " << arr << std::endl;
  // The iterator is still point to index [3] which now holds the value 4
  std::cout << *it << std::endl;
  // Remove a range of elements
  auto start = arr.begin() + 2;  // points to arr[1]
  auto end = arr.end() - 2;      // points to arr[-2] => arr[6]
  arr.erase(start, end);         // zap elements arr[2], arr[3], arr[4], arr[5]
  std::cout << "After erasing range [2, -2): " << arr << std::endl;
  // Remove the last element
  arr.pop_back();
  std::cout << "After popping the last element: " << arr << std::endl;

  // Inserting elements
  // Lets reset the vector
  arr.clear();
  for (int i : {0, 1, 2, 3, 4, 5, 6, 7}) {
    arr.push_back(i);
  }
  arr.insert(arr.begin() + 2, 200);
  std::cout << "After inserting 200 at arr[2]: " << arr << std::endl;
  // Splicing elements
  std::vector<int> vec{1000, 2000, 3000};
  arr.insert(arr.begin() + 2, vec.begin(), vec.end());
  std::cout << "After splicing: " << arr << std::endl;
}

int main(int argc, char** argv) {
  pythonCompat();
  // withIters();
}
