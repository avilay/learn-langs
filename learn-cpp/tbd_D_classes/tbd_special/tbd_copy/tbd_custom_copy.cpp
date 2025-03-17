/*
 * Read default_copy.cppy first.
 * Here I implement a custom copy ctor and custom copy assignment operator both
 * of which do a deep copy.
 */
#include <iostream>
#include <sstream>

unsigned int addrin(void *ptr) {
  std::stringstream ss;
  ss << ptr;
  std::string hexaddr;
  ss >> hexaddr;
  unsigned int addr = std::stoul(hexaddr, nullptr, 16);
  return addr;
}

template <class T> class List {
  T *m_list;
  int m_capacity;
  int m_size;

public:
  List(int capacity);
  List(List &copy);
  ~List();
  void append(T element);
  T get(int idx);
  void set(int idx, T element);
  int getSize();
  const std::string &repr();
  List<T> &operator=(const List &rhs);
};

template <class T> List<T>::List(int capacity) {
  m_capacity = capacity;
  m_size = 0;
  m_list = new T[m_capacity];
}

template <class T> List<T>::List(List &copy) {
  std::cout << "Copy ctor" << std::endl;
  m_capacity = copy.m_capacity;
  m_size = copy.m_size;
  m_list = new T[m_capacity];
  for (int i = 0; i < m_size; i++) {
    m_list[i] = copy.m_list[i];
  }
}

template <class T> List<T>::~List() { delete[] m_list; }

template <class T> void List<T>::append(T element) {
  if (m_size == m_capacity) {
    T *newlist = new T[2 * m_capacity];
    for (int i = 0; i < m_size; i++) {
      newlist[i] = m_list[i];
    }
    delete[] m_list;
    m_list = newlist;
    m_capacity = 2 * m_capacity;
  }
  m_list[m_size] = element;
  m_size += 1;
}

template <class T> T List<T>::get(int idx) {
  if (idx < 0 || idx >= m_size)
    throw "illegal index error!";
  return m_list[idx];
}

template <class T> void List<T>::set(int idx, T element) {
  if (idx < 0 || idx >= m_size)
    throw "illegal index error!";
  m_list[idx] = element;
}

template <class T> int List<T>::getSize() { return m_size; }

template <class T> const std::string &List<T>::repr() {
  std::string &ret = *(new std::string("<List("));
  ret += "capacity=" + std::to_string(m_capacity) + " ";
  ret += "size=" + std::to_string(m_size) + " ";
  ret += "addr=" + std::to_string(addrin(m_list)) + " ";
  ret += "[ ";
  for (int i = 0; i < m_size; i++) {
    ret += std::to_string(m_list[i]) + " ";
  }
  ret += "]";
  ret += ")>";
  return ret;
}

template <class T> List<T> &List<T>::operator=(const List &rhs) {
  delete[] m_list;

  m_capacity = rhs.m_capacity;
  m_size = rhs.m_size;
  m_list = new T[m_capacity];
  for (int i = 0; i < m_size; i++) {
    m_list[i] = rhs.m_list[i];
  }
  return *this;
}

void copy() {
  List<int> obj{10};
  obj.append(10);
  obj.append(20);
  std::cout << "\nOriginal obj(" << std::to_string(addrin(&obj))
            << "): " << obj.repr() << std::endl;

  // This will call the copy ctor which does a deep copy so the underlying list
  // addresses will be different.
  List<int> cpy(obj);
  std::cout << "\nCopy cpy(" << std::to_string(addrin(&cpy))
            << "): " << cpy.repr() << std::endl;

  // This will change the first element of obj but will leave cpy alone
  cpy.set(0, 100);
  std::cout << "\nCopy obj: " << cpy.repr() << std::endl;
  std::cout << "Original obj: " << obj.repr() << std::endl;

  // In contrast to the default_copy_ctr, the end of this function call will not
  // result in a runtime malloc error.
}

void assign() {
  List<int> obj1{5};
  obj1.append(10);
  obj1.append(20);
  std::cout << "\nobj1(" << std::to_string(addrin(&obj1))
            << "): " << obj1.repr() << std::endl;

  // As can be seen from the output, obj2 is a completely different object with
  // a different underlying list pointer.
  List<int> obj2{10};
  std::cout << "\nobj2(" << std::to_string(addrin(&obj2))
            << "): " << obj2.repr() << std::endl;

  // Now because of the custom operator=, the underlying list is also different.
  obj2 = obj1;
  std::cout << "\nobj2(" << std::to_string(addrin(&obj2))
            << "): " << obj2.repr() << std::endl;

  // And we don't get the malloc error.
}

int main() {
  // copy();
  assign();
}
