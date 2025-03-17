#include <iostream>

using namespace std;

struct Cookie {
  int value;
  int getValue();
  void setValue(int);
};

int Cookie::getValue() {
  return value;
}

void Cookie::setValue(int val) {
  value = val;
}

struct Cake {
  double getValue();
  void setValue(double);
};

double Cake::getValue() {
  return 3.141;
}

void Cake::setValue(double dummy) {}

// instead of using <class T> I could've also used <typename T> - both are
// equivalent
template <class T>
T add(T a, T b) {
  // The type T has to support .getValue and .setValue methods. Further the
  // return types of .getValue must support operator+
  // In this sense, templated functions use "compile time" duck types.
  T answer;
  answer.setValue(a.getValue() + b.getValue());
  return answer;
}

int main() {
  // pass
  auto c1 = Cookie();
  c1.setValue(10);

  auto c2 = Cookie();
  c2.setValue(20);

  // strictly speaking I should have called add like so -
  // auto newCookie = add<Cookie>(c1, c2);
  // but in this case it clear from the context which template to use.
  auto newCookie = add(c1, c2);
  cout << newCookie.getValue() << endl;

  auto cake = add(Cake(), Cake());
}