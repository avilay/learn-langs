/*
 * Given an array of **non-negative** numbers and a **non-negative** number k,
 * find the number of subarrays having some specific sum. The basic idea is to
 * use two pointers, with both starting on the 0th element. In the general case
 * when the "left" pointers is on i and *right* pointer is on j, then the
 * current sum is [x_i + x_(i+1) + x_(i+2) + ... + x_j]. If the current sum is
 * less than what we are looking for, then move the "right" pointer to the
 * right, given the array is made up of positive integers, this is bound to
 * increase the current sum. If the current sum is greater than what we are
 * looking for, then move the "left" pointer to the right, this is guaranteed to
 * reduce the current sum.
 */
#include <functional>
#include <iostream>
#include <sstream>
#include <vector>

// Helper functions to get user input from the CLI
#pragma region
template <typename T>
std::ostream &operator<<(std::ostream &out, std::vector<T> arr) {
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

TokensPtr split(const std::string &input, const std::string &delim) {
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

template <typename T>
std::vector<T> convert(const Tokens &tokens,
                       std::function<T(const std::string &)> convertSingle) {
  auto vec = std::vector<T>{};
  for (auto token : tokens) {
    T val = convertSingle(token);
    vec.push_back(val);
  }
  return vec;
}

std::pair<std::vector<int>, int> getUserInput() {
  auto input = std::string{};

  std::cout << "Target value: " << std::endl;
  std::getline(std::cin, input);
  int k = std::stoi(input);

  input.clear();
  std::cout << "Input vector:" << std::endl;
  std::getline(std::cin, input);
  auto tokens = split(input, " ");
  std::vector<int> arr = convert<int>(
      *tokens, [](const std::string &str) { return std::stoi(str); });

  return std::pair(arr, k);
}
#pragma endregion

/*
 * If our current sum is equal to k, we found a sub sequence. If it is less than
 * k, then we move the right pointer right, if it is greater than k we move the
 * left pointer right. In case the sum is equal to k, we can move the left
 * pointer right, because any other movement is going to increase the sum.
 */
int countSubSeqEqualTo(const std::vector<int> &arr, int k) {
  int i = 0, j = 0, count = 0;
  int sum = arr[0];
  while (i < arr.size() && j < arr.size()) {
    if (sum < k) {
      j += 1;
      sum += arr[j];
    } else if (sum == k) {
      count += 1;
      sum -= arr[i];
      i += 1;
    } else {
      sum -= arr[i];
      i += 1;
    }

    // if for some reason the left pointer has overtaken the right pointer, it
    // can only have done so by at most 1 element. Lets just move the right
    // pointer over as well so both of them are pointing to the same element.
    if (i > j) {
      j += 1;
      sum += arr[j];
    }
  }
  return count;
}

/*
 * This is a slightly trickier version of the coutSubSeqsEqualTo(). In this, if
 * the sum(x_i, x_j) is less than the target, it means that all sub sequences in
 * this interval have sums less than the target. Lets consider this sequence
 * [x_11, x_12, x_13, x_14]. The total number of sub sequences here is - 4
 * {x_11}, {x_11, x_12}, {x_11, x_12, x_13}, {x_11, x_12, x_13, x_14}
 * + 3       {x_12}, {x_12, x_13}, {x_13, x_14}
 * + 2           {x_13}, {x_13, x_14}
 * + 1               {x_14}
 *
 * But because we just moved the "right" pointer right by 1, we have already
 * counted all the sub sequences in {x_11, x_12, x_13}. We just need to account
 * for the addition of x_14 to this list. So really we just need to add 4 (the
 * length of the list) to our previous count.
 */
int countSubSeqsLessThan(const std::vector<int> &arr, int k) {
  int i = 0, j = 0, count = 0;
  int sum = arr[0];
  while (i < arr.size() && j < arr.size()) {
    if (sum < k) {
      count += j - i + 1;
      j += 1;
      sum += arr[j];
    } else {
      sum -= arr[i];
      i += 1;
    }

    // if for some reason the left pointer has overtaken the right pointer, it
    // can only have done so by at most 1 element. Lets just move the right
    // pointer over as well so both of them are pointing to the same element.
    if (i > j) {
      j += 1;
      sum += arr[j];
    }
  }
  return count;
}

void cli() {
  auto tpl = getUserInput();
  std::vector<int> arr = tpl.first;
  int k = tpl.second;
  int n_subseqs = countSubSeqsLessThan(arr, k);
  std::cout << "Number of subseqs less than " << k << " are " << n_subseqs
            << std::endl;
}

void tests() {
  std::vector<int> arr{2, 5, 6};
  int exp_count = 4;
  int act_count = countSubSeqsLessThan(arr, 10);
  assert(exp_count == act_count);
  std::cout << "1. Pass" << std::endl;

  arr.clear();
  arr.insert(arr.begin(), {1, 11, 2, 3, 15});
  act_count = countSubSeqsLessThan(arr, 10);
  assert(exp_count == act_count);
  std::cout << "2. Pass" << std::endl;

  arr.clear();
  arr.insert(arr.begin(), {9, 4, 20, 3, 10, 5});
  exp_count = 2;
  act_count = countSubSeqEqualTo(arr, 33);
  assert(exp_count == act_count);
  std::cout << "3. Pass" << std::endl;

  // Look for sums that exist on the edges
  exp_count = 2;
  act_count = countSubSeqEqualTo(arr, 13);
  assert(exp_count == act_count);
  std::cout << "4. Pass" << std::endl;

  exp_count = 1;
  act_count = countSubSeqEqualTo(arr, 15);
  assert(exp_count == act_count);
  std::cout << "5. Pass" << std::endl;

  // Look for sums that do not exist beyond the left edge, in the middle, and
  // beyond the right edge
  exp_count = 0;

  act_count = countSubSeqEqualTo(arr, 1);
  assert(exp_count == act_count);
  std::cout << "6. Pass" << std::endl;

  act_count = countSubSeqEqualTo(arr, 55);
  assert(exp_count == act_count);
  std::cout << "7. Pass" << std::endl;

  act_count = countSubSeqEqualTo(arr, 25);
  assert(exp_count == act_count);
  std::cout << "8. Pass" << std::endl;
}

int main(int argc, char **argv) { tests(); }
