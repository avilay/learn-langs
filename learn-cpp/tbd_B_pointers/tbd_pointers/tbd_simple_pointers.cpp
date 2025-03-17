#include <iostream>

using namespace std;

int main() {
  // Simplest case. We have two variables a and b, and a pointer ptr that can
  // point to either of them. We can also change the value of the variables
  // either directly or through the pointer.
  int* ptr = nullptr;
  int a = 42;
  int b = 786;
  ptr = &a;
  cout << "*ptr=" << *ptr << " a=" << a << " b=" << b << endl;
  ptr = &b;
  cout << "*ptr=" << *ptr << " a=" << a << " b=" << b << endl;
  *ptr = 33;
  cout << "*ptr=" << *ptr << " a=" << a << " b=" << b << endl;
  b = 786;
  cout << "*ptr=" << *ptr << " a=" << a << " b=" << b << endl;

  // Now lets declare a pointer that points to a const int.
  // This means that we cannot change the value of the variable through the
  // pointer.
  const int* ptrc = nullptr;
  ptrc = &a;
  cout << "*ptrc=" << *ptrc << " a=" << a << endl;

  // the below line will not compile
  // because the value pointed to by ptrc cannot be altered.
  // *ptrc = 22;
  // Of course I can still change the value from underneath it.
  a = 22;
  cout << "*ptrc=" << *ptrc << " a=" << a << endl;

  // And I can still change the pointer itself to point to some other address
  ptrc = &b;
  cout << "*ptrc=" << *ptrc << " b=" << b << endl;

  // Now lets declare a const pointer that points to an int. It can be assigned
  // when being declared, just like other const variables. And once assigned it
  // can never point to anything else.
  int* const cptr = &a;
  cout << "*cptr=" << *cptr << " a=" << a << endl;

  // The following line will not compile.
  // cptr = &b;

  // But I can still change the value being pointed to. As long as the address
  // it is pointing to is const, i.e., unchanging, the contents of the address
  // can change.
  *cptr = 42;
  cout << "*cptr=" << *cptr << " a=" << a << endl;

  // And finally lets declare a const pointer pointing to a const int. It is the
  // most immutable of pointers.
  const int* const cptrc = &a;
  cout << "*cptrc=" << *cptrc << " a=" << a << endl;

  // The following lines will not compile
  // cptrc = &b;
  // *cptrc = 21;

  return 0;
}