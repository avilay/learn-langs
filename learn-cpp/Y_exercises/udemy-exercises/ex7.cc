#include <iostream>
using namespace std;

const float kWeeklySalary = 600.0;
const float kHourlySalary = 7.0;
const float kHourlyCommision = 0.1;
const float kOnlyCommision = 0.2;
const float kBonus = 20.0;
const float kPrice = 225.0;
const int kHoursPerWeek = 40;

int GetInput() {
  int weekly_sales = 0;
  cout << "How many pairs of shoes will you sell this week? ";
  while (!(cin >> weekly_sales)) {
    cout << "That is not an integer. Try again: ";
    cin.clear();
    cin.ignore(1000, '\n');
  }
  return weekly_sales;
}

float Salary(int weekly_sales) { return kWeeklySalary; }

float SalaryCommission(int weekly_sales) {
  float salary = kHourlySalary * kHoursPerWeek;
  float revenue = weekly_sales * kPrice;
  float commision = revenue * kHourlyCommision;
  return salary + commision;
}

float Commision(int weekly_sales) {
  float revenue = weekly_sales * kPrice;
  float commision = revenue * kOnlyCommision;
  float bonus = weekly_sales * kBonus;
  return commision + bonus;
}

int main() {
  int weekly_sales = GetInput();
  float income_salary = Salary(weekly_sales);
  float income_salary_comm = SalaryCommission(weekly_sales);
  float income_comm = Commision(weekly_sales);
  cout << "Income from salary $" << income_salary << endl;
  cout << "Income from salary and commision $" << income_salary_comm << endl;
  cout << "Income from commission $" << income_comm << endl;
  return 0;
}