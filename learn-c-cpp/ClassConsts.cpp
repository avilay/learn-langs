/*
 * Const objects of classes have only read-access to the public attributes and
 * can call only const methods. Const methods can be overloaded.
 */
#include <iostream>

using namespace std;

class Cookie {
private:
  int caloriesPerServing;

public:
  string flavor; // only read-access for const object

  Cookie(string flavor, int caloriesPerServing)
      : flavor(flavor), caloriesPerServing(caloriesPerServing) {}
  int calories(int servingSize); // this cannot be called by a const object
  int calories(int servingSize) const; // this can be
};

int Cookie::calories(int servingSize) {
  cout << "Cookie::calories" << endl;
  return servingSize * caloriesPerServing;
}

// this method can only call other const methods
// and has only read-access to the data members
int Cookie::calories(int servingSize) const {
  cout << "Cookie::calories const" << endl;
  if (flavor == "Chocolate Chip") {
    return 200 * servingSize;
  } else if (flavor == "Oatmeal Raisin") {
    return 180 * servingSize;
  } else {
    return 220 * servingSize;
  }
}

int main() {
  const Cookie cookie{"Chocolate Chip", 100};
  cout << "cookie.flavor=" << cookie.flavor << endl;

  // the following won't work
  // cookie.flavor = "Oatmeal Raisin";

  // the following will call the const variant
  cout << "calories=" << cookie.calories(2) << endl;
}
