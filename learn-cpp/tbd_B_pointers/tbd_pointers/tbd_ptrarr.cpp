#include <math.h>
#include <iostream>
#include <sstream>

using namespace std;

unsigned int addrin(void* ptr) {
  stringstream ss;
  ss << ptr;
  string hexaddr;
  ss >> hexaddr;
  unsigned int addr = stoul(hexaddr, nullptr, 16);
  return addr;
}

// iterating an array using pointer arithmetic
double l2Norm(double* vec, int len) {
  // remember that incrementing a double pointer by 3 means
  // numerically incrementing it by 3 * sizeof(double).
  double* end = vec + 3;
  double sum = 0;
  for (auto curr = vec; curr < end; curr++) {
    sum += pow(*curr, 2);
  }
  return sqrt(sum);
}

int main() {
  // an array variable is equivalent to a pointer pointing to the first element
  // of the array and a pointer to an array can be treated as an array variable
  // itself!
  int arr[]{1, 2, 3};
  int* ptr = arr;
  cout << "*ptr = " << *ptr << endl;
  cout << "arr[0] = " << arr[0] << endl;
  cout << "*arr = " << *arr << endl;
  cout << "ptr[1] = " << ptr[1] << endl;

  // so what happens if index a regular int pointer?
  // it'll just point to the next chunk of memory and read whatever is in there!
  int v = 42;
  int* bad = &v;
  cout << "bad[0]=" << bad[0] << " bad[1]=" << bad[1] << endl;

  // pointer arithmetic is a bit different than regular arithmetic
  // incrementing/decrementing pointer will increment
  cout << "ptr is pointing to address " << addrin(ptr) << endl;
  ptr = ptr + 1;
  // Now ptr will have incremented by 4, the size of int, the datatype it
  // is pointing to
  cout << "ptr is pointing to address " << addrin(ptr) << endl;

  // the datatype of the pointer itself is a long int.
  // if I increment a pointer-to-a-pointer then it will be incremented by 8.
  int** ptrptr = &ptr;
  cout << "ptrptr is pointing to address " << addrin(ptrptr) << endl;
  ptrptr += 1;
  cout << "ptrptr is pointing to address " << addrin(ptrptr) << endl;

  double vec[] = {1.0, 2.0, 3.0};
  cout << "L2 norm is " << l2Norm(vec, 3) << endl;
}