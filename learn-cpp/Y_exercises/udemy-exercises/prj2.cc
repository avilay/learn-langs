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
  int score1 = GetIntInput("Enter assignemnt1 score: ");
  int score2 = GetIntInput("Enter assignment2 score: ");
  int score3 = GetIntInput("Enter assignment3 score: ");
  int score4 = GetIntInput("Enter assignment4 score: ");
  int midterm = GetIntInput("Enter midterm score: ");
  int final_exam = GetIntInput("Enter final examination score: ");
  int class_part_score = GetIntInput("Enter class participation score: ");
  float assignment_score = (score1 + score2 + score3 + score4) / 4.0;
  float final_grade = (assignment_score * 0.4) + (midterm * 0.15) +
                      (final_exam * 0.35) + (class_part_score * 0.1);
  cout << "Final grade is : " << final_grade << endl;
  return 0;
}
