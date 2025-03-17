/*
 * Lets say I have already have an object of type MyClass -
 * MyClass obj;
 *
 * Copy constructors are invoked in the following usage scnearios:
 * MyClass copy_obj(obj);
 *
 * While the below does not look like a copy ctor, remember this is invoking the
 * single param ctor.
 * MyClass copy_obj = obj;
 *
 * Conversely, while the below looks like a copy ctor, it is simply copying the
 * pointer value
 * MyClass *ptr(&obj);
 *
 * Just like the default ctor, the compiler will provide a copy ctor which will
 * do a shallow copy of the object.
 *
 * A related special memeber is the copy assignment operator. It is called in
 * the following usage scenarios. First obj2 is instantiated in the usual way.
 * It is a completely different object with a completely different underlying
 * list pointer. Then when it is assigned to obj, the copy assignment operator
 * is called. As usual, the default compiler provided assignment operator will
 * do a shallow copy. Which means that obj2, while still being a separate
 * object, will now share the underlying list pointer with obj.
 *
 * MyClass obj2;
 * obj2 = obj;
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
  ~List();
  void append(T element);
  T get(int idx);
  void set(int idx, T element);
  int getSize();
  const std::string &repr();
};

template <class T> List<T>::List(int capacity) {
  m_capacity = capacity;
  m_size = 0;
  m_list = new T[m_capacity];
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

void ctorWithObjects() {
  List<int> obj{10};
  obj.append(10);
  obj.append(20);
  std::cout << "\nOriginal obj(" << std::to_string(addrin(&obj))
            << "): " << obj.repr() << std::endl;

  // Default copy ctor called here which will do a shallow copy. As can be seen
  // from the object addresses, the top level objects are different. But as can
  // be seen from the repr output, the underlying list addresses are the same.
  List<int> cpy(obj);
  std::cout << "\nCopy cpy(" << std::to_string(addrin(&cpy))
            << "): " << cpy.repr() << std::endl;

  // This will change the first element of both the obj and cpy.
  cpy.set(0, 100);
  std::cout << "\nCopy obj: " << cpy.repr() << std::endl;
  std::cout << "Original obj: " << obj.repr() << std::endl;

  // This will seem to leave original obj alone but that is only because m_size
  // of both the objects differ. When a new element is appended, even though the
  // common underlying list gets the new element, the m_size is only updated for
  // the cpy object.
  cpy.append(30);
  std::cout << "\nCopy obj: " << cpy.repr() << std::endl;
  std::cout << "Original obj: " << obj.repr() << std::endl;

  // At the end of this function, the CRT will try to destroy both the objects
  // will get an error when attempting to destroy the second object because the
  // underlying list pointer will have already been destroyed by the first
  // object's dtor.
}

void assignWithObjects() {
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

  // But after the assignment, while obj2 itself is the same as above, the
  // underlying list poitner is shared by both obj1 and obj2.
  obj2 = obj1;
  std::cout << "\nobj2(" << std::to_string(addrin(&obj2))
            << "): " << obj2.repr() << std::endl;

  // As usual, because both the objects have the same underlying pointer object,
  // we'll get the malloc error.
}

void ctorWithPointers() {
  List<int> *ptr = new List<int>{10};
  ptr->append(10);
  ptr->append(20);

  // this might seem like the copy ctor is being called, but actually it is just
  // copying the pointer values. The addr in cpy is the same as that in ptr.
  List<int> *cpy(ptr);
  std::cout << "Original ptr(" << std::to_string(addrin(ptr))
            << "): " << ptr->repr() << std::endl;
  std::cout << "Copy cpy(" << std::to_string(addrin(cpy)) << "):" << cpy->repr()
            << std::endl;

  List<int> *cpy2 = ptr; // this is what happened
  std::cout << "Second copy cpy2(" << std::to_string(addrin(cpy))
            << "):" << cpy->repr() << std::endl;

  // this is actually invoking the default copy ctor
  List<int> cpy_obj(*ptr);
  std::cout << "Original ptr(" << std::to_string(addrin(ptr))
            << "): " << ptr->repr() << std::endl;
  std::cout << "Copy obj(" << std::to_string(addrin(&cpy_obj))
            << "):" << cpy_obj.repr() << std::endl;
}

int main() {
  // ctorWithObjects();
  // assignWithObjects();
  ctorWithPointers();
}