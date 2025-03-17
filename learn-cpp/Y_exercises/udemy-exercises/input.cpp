#include <iostream>
using namespace std;

int simple_main() {
  int answer = 0;
  do {
    cout << "Enter a number (-1 to quit): ";
    if (!(cin >> answer)) {
      cout << "You entered a non-numeric. Exiting." << endl;
      break;
    }
    if (answer != -1) {
      cout << "You entered " << answer << endl;
    }
  } while (answer != -1);
  cout << "All done." << endl;
  return 0;
}

int dbg_main() {
  int answer = 0;
  do {
    cout << "Enter a number (-1 to quit): ";

    // http://www.cplusplus.com/reference/istream/istream/operator%3E%3E/
    istream &mycin = cin >> answer;

    // http://www.cplusplus.com/reference/ios/ios/rdstate/
    cout << mycin.rdstate() << endl;
    if (mycin.good()) {
      cout << "cin is in good state" << endl;
    } else if (mycin.eof()) {
      cout << "cin is in EOF state" << endl;
    } else if (mycin.fail()) {
      cout << "cin is in fail state" << endl;
    } else if (mycin.bad()) {
      cout << "cin is in bad state" << endl;
    }

    // http://www.cplusplus.com/reference/ios/ios/operator_not/
    if (!(mycin)) {
      cout << "You entered a non-numeric. Exiting." << endl;
      break;
    }
    if (answer != -1) {
      cout << "You entered " << answer << endl;
    }
  } while (answer != -1);
  cout << "All done." << endl;
  return 0;
}

int main() {
  int answer = 0;
  do {
    cout << "Enter a number (-1 to quit): ";
    if (!(cin >> answer)) {
      cin.clear();
      cin.ignore(10000, '\n');
      cout << "You entered a non-numeric entry. Try again." << endl;
    } else if (answer != -1) {
      cout << "You entered " << answer << endl;
    }
  } while (answer != -1);
  cout << "All done." << endl;
  return 0;
}