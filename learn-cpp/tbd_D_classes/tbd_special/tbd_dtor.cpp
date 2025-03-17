#include <iostream>
using namespace std;

template <class T> class List {
  T *list;
  int capacity;
  int size;

public:
  List(int capacity) : capacity(capacity), size(0) { list = new T[capacity]; }
  void append(T element);
  T get(int idx);
  ~List();
};

template <class T> void List<T>::append(T element) {
  if (size == capacity) {
    T *newlist = new T[2 * capacity];
    for (int i = 0; i < size; i++) {
      newlist[i] = list[i];
    }
    delete[] list;
    list = newlist;
    capacity = 2 * capacity;
  }
  list[size] = element;
  size += 1;
}

template <class T> T List<T>::get(int idx) {
  if (idx < 0 || idx >= size) {
    throw "illegal index error!";
  }
  return list[idx];
}

template <class T> List<T>::~List() {
  cout << "Inside dtor" << endl;
  delete[] list;
}

int main_1() {
  List<int> list{3};
  list.append(10);
  list.append(20);
  list.append(30);

  // Will not work. Cannot delete objects on the stack
  // or rather objects that were not allocated with a new
  // delete list;
  cout << list.get(0) << endl;
  return 0;
}

int main() {
  List<int> *list = new List<int>{3};
  list->append(10);
  list->append(20);
  list->append(30);
  cout << list->get(0) << endl;

  delete list;

  // interestingly enough this still works on my Mac
  // it will output 0 (the default int value)
  cout << list->get(0) << endl;
}
