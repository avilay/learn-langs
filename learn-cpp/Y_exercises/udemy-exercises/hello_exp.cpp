#include <iomanip>
#include <iostream>
using namespace std;

int main_1() {
  for (int i = 0; i < 6; i++) {
    for (int j = 0; j < 4; j++) {
      // cout << "   Hello World!  ";
      // Insteawd use http://www.cplusplus.com/reference/iomanip/setw/
      cout << setw(17) << "Hello World!";
    }
    cout << endl;
  }
  return 0;
}

int main() {
  for (int i = 0; i < 6; i++) {
    for (int j = 0; j < 4; j++) {
      cout << left << setw(17) << "Hello World!";
    }
    cout << endl;
  }
  return 0;
}