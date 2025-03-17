#include <float.h>
#include <math.h>
#include <array>
#include <iostream>

using namespace std;

double l2Norm(double vec[], int len) {
  // Cannot do range based loop here because the compiler does not know the
  // length
  double sum = 0;
  for (int idx = 0; idx < len; idx++) {
    sum += pow(vec[idx], 2);
  }
  return sqrt(sum);
}

double maxNorm(array<double, 3> vec) {
  double maxVal = DBL_MIN;
  for (auto x : vec) {
    if (x > maxVal)
      maxVal = x;
  }
  return maxVal;
}

int main() {
  double nums[] = {1.0, 2.0, 3.0};
  // Can do range based loop here because compiler knows the length of the array
  for (auto n : nums) {
    cout << n << " ";
  }
  cout << endl;

  auto l2n = l2Norm(nums, 3);
  cout << "L2 Norm of above vector is " << l2n << endl;

  array<double, 3> v3d{1.0, 2.0, 3.0};
  cout << "size of the vector is " << v3d.size() << endl;
  auto mn = maxNorm(v3d);
  cout << "Max Norm of above vector is " << mn << endl;

  // Dynamic arrays are arrays whose length is not known at compile time.
  // These have to be initialized on the heap
  int len;
  cout << "Enter length of array: " << endl;
  cin >> len;
  // The following will not compile
  // int nums2[len]{};
  int* ptr = new int[len];
  for (int i = 0; i < len; i++) {
    cout << ptr[i] << " ";
  }
  cout << endl;

  return 0;
}