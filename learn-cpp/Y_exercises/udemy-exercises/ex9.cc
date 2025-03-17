#include <fstream>
#include <iostream>
using namespace std;

int write() {
  string first_name;
  string last_name;
  int age;
  string file_name;

  // Collect the data
  cout << "Enter First Name: ";
  cin >> first_name;

  cout << "Enter Last Name: ";
  cin >> last_name;

  cout << "Enter Age: ";
  cin >> age;

  cout << endl << "Enter name of the file: ";
  cin >> file_name;

  ofstream people(file_name, ios::out | ios::app);
  people << first_name << endl << last_name << endl << age << endl;
  people.close();
  return 0;
}

int read() {
  string filename;
  cout << "Enter file name: ";
  cin >> filename;

  ifstream family(filename);
  string first_name, last_name;
  int age;
  family >>
      first_name; // similar to readline but does not read in the trailing \n
  family >> last_name;
  family >> age;
  cout << first_name << " " << last_name << " " << age << endl;

  string last_empty_line = "EMPTY LINE";
  // This does not put anyting in the last_empty_line
  family >> last_empty_line;
  cout << "last empty line: " << last_empty_line << endl;

  if (family.eof()) {
    cout << "EOF reached" << endl;
  } else {
    cout << "EOF NOT reached!" << endl;
  }
  family.close();
  return 0;
}

int write_many() {
  string filename;
  cout << "Enter filename: ";
  cin >> filename;
  ofstream datafile(filename, ios::out | ios::app);

  string fname, lname;
  int age;
  bool done = false;
  while (!done) {
    cout << "Enter first name: ";
    cin >> fname;

    cout << "Enter last name: ";
    cin >> lname;

    cout << "Enter age: ";
    cin >> age;

    datafile << fname << "," << lname << "," << age << endl;
    cout << "Continue (Y/n)? ";
    string ans;
    cin >> ans;
    if (ans == "n")
      done = true;
  }
  datafile << endl;
  datafile.close();
  return 0;
}

int read_many() {
  string filename;
  cout << "Enter file name: ";
  cin >> filename;
  ifstream datafile(filename);
  string record;
  while (!datafile.eof()) {
    datafile >> record;
    cout << record << endl;
    // clear the variable because the last empty line will not read in anything.
    record = "";
  }
  datafile.close();
  return 0;
}

int main() { return read_many(); }