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

int main() {
  int tot_secs = GetIntInput("Enter number of seconds: ");
  int secs = tot_secs % 60;
  int tot_mins = tot_secs / 60;
  int mins = tot_mins % 60;
  int tot_hrs = tot_mins / 60;
  cout << "Hours : " << tot_hrs << endl;
  cout << "Minutes: " << mins << endl;
  cout << "Seconds: " << secs << endl;

  return 0;
}