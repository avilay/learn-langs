#include <iostream>
using namespace std;

string GetInput(string prompt) {
  string inval;
  cout << prompt;
  cin >> inval;
  return inval;
}

int DecodeMonth(char encoded_month) {
  // A -> Jan
  int month = ((int)encoded_month - (int)'A') + 1;
  if (month < 1 || month > 12) {
    cout << "Invalid month!" << endl;
    return -1;
  }
  return month;
}

int DecodeDay(string encoded_day) {
  // Q -> 0
  int digit_0 = (int)(encoded_day[0] - (int)'Q');
  int digit_1 = (int)(encoded_day[1] - (int)'Q');
  int day = digit_0 * 10 + digit_1;
  if (day < 1 || day > 31) {
    cout << "Invalid day!" << endl;
    return -1;
  }
  return day;
}

int DecodeYear(char encoded_year) {
  // A -> 1
  int year_offset = (int)(encoded_year - (int)'A') + 1;
  int year = 1995 + year_offset;
  return year;
}

int main() {
  // string encoded_date = GetInput("Enter encoded date: ");
  string encoded_date = "ARZM";

  char encoded_month = encoded_date[0];
  string encoded_day = "QQ"; // initlized to 00
  char encoded_year;

  if (encoded_date.length() == 3) {
    encoded_day[1] = encoded_date[1];
    encoded_year = encoded_date[2];
  } else if (encoded_date.length() == 4) {
    encoded_day[0] = encoded_date[1];
    encoded_day[1] = encoded_date[2];
    encoded_year = encoded_date[3];
  } else {
    cout << "Invalid date!" << endl;
    return -1;
  }

  int month = DecodeMonth(encoded_month);
  if (month == -1) {
    return -1;
  }

  int day = DecodeDay(encoded_day);
  if (day == -1) {
    return -1;
  }

  int year = DecodeYear(encoded_year);
  if (year == -1) {
    return -1;
  }

  cout << year << "-" << month << "-" << day << endl;
  return 0;
}