#include <iostream>

using namespace std;

struct Cookie {
  int calories;
  string flavor;

  string repr();
};

string Cookie::repr() {
  string ret = "<Cookie(";
  ret += "calories=" + to_string(calories) + " flavor=" + flavor;
  ret += ")>";
  return ret;
}

long long int getInput(const string& prompt) {
  long long int input = 0;
  cout << prompt;
  while (!(cin >> input)) {
    cout << "That is not an integer. Try again: ";
    cin.clear();
    cin.ignore(1000, '\n');
  }
  return input;
}

int main() {
  // The following object is allocated on the stack
  // because the compiler knows the size
  Cookie cookieObj = Cookie();

  // allocated on the heap and will return a pointer
  // because of the new keyword
  Cookie* cookiePtr = new Cookie();

  cookieObj.calories = 200;
  cookieObj.flavor = "Chocolate Chip";
  cout << cookieObj.repr() << endl;

  cookiePtr->calories = 120;
  cookiePtr->flavor = "Oatmeal Raisin";
  cout << cookiePtr->repr() << endl;

  // After I am done with the object on the heap, free the memory
  delete cookiePtr;

  // Compiler knows to allocate two cookie objects on the stack
  Cookie cookies[] = {Cookie(), Cookie()};
  cookies[0].calories = 220;
  cookies[0].flavor = "Snicker Doodle";
  cookies[1].calories = 180;
  cookies[1].flavor = "Double Chocolate Chip";
  for (auto cookie : cookies) {
    cout << cookie.repr() << endl;
  }

  // To create a dynamically sized array, we have to resort to new
  // and the objects will have to be allocated on the heap
  cout << "\nDynamic allocation" << endl;
  long long int numCookies = getInput("How many cookies do you want? ");
  try {
    Cookie* cookiePtr2 = new Cookie[numCookies];

    cout << "Creating cookies" << endl;
    for (Cookie* curr = cookiePtr2; curr < cookiePtr2 + numCookies; curr++) {
      curr->calories = rand() % 200;
      curr->flavor = "Semi Sweet Chocolate Chip";
    }

    if (numCookies < 10) {
      cout << "Printing out cookies" << endl;
      for (int i = 0; i < numCookies; i++) {
        cout << cookiePtr2[i].repr() << endl;
      }
    }

    // Just calling delete[] will free the memory. The call to new stores the
    // number of elements in the header.
    string dummy;
    cout << "Press ENTER to start deleting cookies: " << endl;
    cin >> dummy;
    cout << "Deleting cookies" << endl;
    delete[] cookiePtr2;
  } catch (bad_alloc& oom) {
    cout << "You cannot have so many cookies!" << endl;
  }
}