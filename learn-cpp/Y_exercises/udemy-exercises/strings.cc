#include <iostream>
using namespace std;

int main() {
  string str1 = "To be or not to be, that is the question";
  string str2 = "only ";
  string str3 = str1.substr(6, 12);
  str1.insert(32, str2);
  cout << "str1 = " << str1 << endl;
  cout << "str3 = " << str3 << endl;
  str1.replace(str1.find("to be", 0), 5, "to jump");
  cout << "str1 = " << str1 << endl;
  str1.erase(9, 4);
  string str4 = str1.substr(6, 12);
  cout << "str1 = " << str1 << endl;
  cout << "str3 = " << str3 << endl;
  cout << "str4 = " << str4 << endl;
}
