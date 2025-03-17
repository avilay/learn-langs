#include <iostream>

using namespace std;

enum class Color { RED, GREEN, BLUE };

void prettyPrint(const string& msg, Color color) {
  if (color == Color::RED) {
    cout << "\033[31m" << msg << "\033[0m" << endl;
  } else if (color == Color::GREEN) {
    cout << "\033[32m" << msg << "\033[0m" << endl;
  } else if (color == Color::BLUE) {
    cout << "\033[34m" << msg << "\033[0m" << endl;
  } else {
    throw "bad color";
  }
}

int main() {
  Color color = Color::GREEN;
  prettyPrint("Hello there", color);
}
