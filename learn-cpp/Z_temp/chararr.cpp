#include <iostream>

using namespace std;

int main() {
  // char arrays can be initlized in two different ways -
  char name1[] = {'a', 'v', 'i', 'l', 'a', 'y'};
  int idx = 0;
  for (auto c : name1) {
    cout << idx << " " << c << " ";
    idx++;
  }
  cout << endl;

  char name2[] = "avilay"; // size is 7 includes a trailing \0 element.
  idx = 0;
  for (auto c : name2) {
    cout << idx << " " << c << " ";
    idx++;
  }
  cout << endl;

  // null terminated char arrays can be passed to cout directly
  cout << "null-terminated char array to cout: " << name2 << endl;

  // but non-null terminated char arrays can lead to weird behavior because the
  // crt will look for the null character to stop iterating
  cout << "non-null-terminated char array to cout: " << name1 << endl;

  // It is easy to change null-terminated char arrays to strings and vice-versa
  string n2 = name2;
  cout << "null-terminated char array to string: " << n2 << endl;

  string n1 = name1;
  cout << "non-null-terminated char array to string: " << n1 << endl;

  // But remember that these are ways to **initialize**, they cannot be used for
  // assignment The following will not compile
  // name1 = "anika";
  // I can ofcourse change the individual elements
  name1[0] = 'a';
  name1[1] = 'n';
  name1[2] = 'i';
  name1[3] = 'k';
  name1[4] = 'a';
  name1[5] = '\0';
  idx = 0;
  for (auto c : name1) {
    cout << idx << " " << c << " ";
    idx++;
  }
  cout << endl;
}
