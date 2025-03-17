#include <iostream>
using namespace std;

/*
 * Cookie has no explicit ctor defined, because of that the compiler will create
 * a default ctor that does nothing. It is equivalent to - Cookie() {}
 */

class Cookie {
  int calories;
  string flavor;

public:
  void setCalories(int calories);
  void setFlavor(const string &flavor);
  string repr();
};

void Cookie::setCalories(int calories) { this->calories = calories; }
void Cookie::setFlavor(const string &flavor) { this->flavor = string(flavor); }

string Cookie::repr() {
  string ret = "<Cookie(";
  ret += "calories=" + to_string(calories) + " ";
  ret += "flavor=" + flavor;
  ret += ")>";
  return ret;
}

/*
 * Cake on the other had does have some ctor defined, even though it is not the
 * default ctor, compiler will not create a default ctor!
 */

class Cake {
  int weight;
  string base;
  string icing;

public:
  Cake(int weight);
  void setFlavor(const string &base, const string &icing);
  string repr();
};

Cake::Cake(int weight) {
  this->weight = weight;
  base = "plain";
  icing = "vannila";
}

void Cake::setFlavor(const string &base, const string &icing) {
  this->base = base;
  this->icing = icing;
}

string Cake::repr() {
  string ret = "<Cake(";
  ret += "weight=" + to_string(weight) + " ";
  ret += "base=" + base + " ";
  ret += "icing=" + icing;
  ret + ")>";
  return ret;
}

/*
 * Of course I can explicitly define a default ctor in such cases.
 */
class Pizza {
  int size;
  bool isThinCrust;
  string sauce;

public:
  Pizza() : size(6), isThinCrust(true), sauce("Marinara") {}
  Pizza(int size);
  void setSauce(const string &sauce);
  void setCrust(bool isThin);
  const string &repr();
};

Pizza::Pizza(int size) {
  this->size = size;
  isThinCrust = true;
  sauce = "Marinara";
}

void Pizza::setSauce(const string &sauce) { this->sauce = string(sauce); }

void Pizza::setCrust(bool isThin) { isThinCrust = isThin; }

const string &Pizza::repr() {
  // string ret = "<Pizza(";
  string *pret = new string("<Pizza(");
  string &ret = *pret;
  ret += "size=" + to_string(size) + " ";
  ret += "isThinCrust=" + to_string(isThinCrust) + " ";
  ret += "sauce=" + sauce;
  ret += ")>";
  return ret;
}

int main() {
  Cookie cc; // default ctor called;
  cc.setFlavor("Chocolate Chip");
  cc.setCalories(200);
  cout << cc.repr() << endl;

  // The following line will not compile because it is calling the default ctor
  // which is not defined.
  // Cake cake;
  Cake cake{2};
  cout << cake.repr() << endl;

  // Will invoke the explicitly defined default ctor
  Pizza pizza;
  cout << pizza.repr() << endl;
}
