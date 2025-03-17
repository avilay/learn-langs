#include <iostream>
using namespace std;

int GetIntInput(string prompt) {
  cout << prompt;
  int inval;
  while (!(cin >> inval)) {
    cout << "That is not an integer. Try again: ";
    cin.clear();
    cin.ignore(1000, '\n');
  }
  return inval;
}

int Reverse(int n) {
  int digits[3];
  digits[0] = n % 10;
  n = (int)(n / 10);
  digits[1] = n % 10;
  n = (int)(n / 10);
  digits[2] = n;
  int rev_n = digits[0] * 100 + digits[1] * 10 + digits[2];
  return rev_n;
}

int main() {
  int n = GetIntInput("Enter a 3-digit number: ");
  int n_rev = Reverse(n);
  int diff = n - n_rev;
  int diff_rev = Reverse(diff);
  int final_ans = diff + diff_rev;
  cout << "Final answer is " << final_ans << endl;
  return 0;
}
