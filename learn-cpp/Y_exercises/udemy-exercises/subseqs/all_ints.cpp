/*
 * Given an unordered array of integers and an integer k - both can be positive
 * or negative - find the number of subarrays having some specific sum. The
 * basic idea is to calcualte the cumulative sums s_n = x_0 + x_1 + ... + x_n.
 * Then if s_n == k, we have found a sub sequence. Other than that, if we find
 * two cumulative sums s_m and s_n s.t., s_m - s_n = k then the sub sequence
 * [x_{n+1}, x_m] adds up to k provided m > n of course.
 */

#include <iostream>
#include <unordered_map>
#include <vector>

int countSubSeqEqualTo(const std::vector<int> &arr, int k) {
  std::unordered_map<int, int> cumsums{};
  int cumsum = 0;
  int counts = 0;
  for (auto x : arr) {
    cumsum += x;
    if (cumsum == k) {
      counts += 1;
    }

    // Think of cumsum as s_m and target as s_n. Because we are building
    // cumsums progressively, trying to find s_n in cumsums ensures that m >
    // n.
    int target = cumsum - k;
    counts += cumsums[target];

    cumsums[cumsum] += 1;
  }
  return counts;
}

void test() {
  std::vector<int> arr{10, 2, -2, -20, 10};
  int exp_count = 3;
  int act_count = countSubSeqEqualTo(arr, -10);
  assert(exp_count == act_count);

  arr.clear();
  arr.insert(arr.begin(), {9, 4, 20, 3, 10, 5});
  exp_count = 2;
  act_count = countSubSeqEqualTo(arr, 33);
  assert(exp_count == act_count);
}

int main() { test(); }