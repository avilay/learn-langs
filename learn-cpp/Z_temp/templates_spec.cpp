/*
 * This is weird. Don't understand this entirely.
 */

#include <iostream>
using namespace std;

template <class T> class MyContainer {
  T element;

public:
  MyContainer(T arg) : element(arg) {}
  T increase();
};

template <class T> T MyContainer<T>::increase() { element += 1; }

template <> class MyContainer<char> {
  char element;

public:
  MyContainer(char arg) : element(arg) {}
  char increase();
  char upper();
};

template <> char MyContainer<char>::increase() { element += 1; }
