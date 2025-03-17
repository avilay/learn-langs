#include <iostream>
using namespace std;

int GetIntInput(string prompt) {
  int inval;
  cout << prompt;
  while (!(cin >> inval)) {
    cout << "That is not an integer. Try again: ";
    cin.clear();
    cin.ignore(1000, '\n');
  }
  return inval;
}

int main() {
  int num_chirps = GetIntInput("Enter number of chirps: ");
  float temp = (num_chirps + 40) / 4.0;
  cout << "The temperature is: " << temp << endl;
}
