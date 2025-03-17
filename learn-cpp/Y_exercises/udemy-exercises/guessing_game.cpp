#include <iostream>
#include <time.h>
using namespace std;

void learn_rand() {
  srand(time(NULL));

  int counts[100] = {};
  for (int i = 0; i < 1000; i++) {
    int n = rand() % 100;
    counts[n] += 1;
  }

  for (int i = 0; i < 100; i++) {
    cout << i << "->" << counts[i] << endl;
  }
}

int Input() {
  int inval = 0;
  while (!(cin >> inval)) {
    cout << "That is not an number. Try again: ";
    cin.clear();
    cin.ignore(1000, '\n');
  }
  return inval;
}

int main() {
  srand(time(NULL));
  int answer = rand() % 100;
  int guess = -1;
  int tries = 0;
  int max_tries = 10;
  do {
    cout << "Guess the number: ";
    guess = Input();
    if (guess < answer) {
      cout << "That is too low. ";
    } else if (guess == answer) {
      cout << "That is the correct answer! ";
    } else {
      cout << "That is too high. ";
    }
    tries += 1;
  } while ((guess != answer) && (tries < max_tries));

  if (guess == answer) {
    cout << "YOU WIN!!" << endl;
  } else {
    cout << "You ran out of chances. You LOSE! The correct answer is " << answer
         << endl;
  }

  return 0;
}
