/*
 * !!!! THIS WRITEUP IS NOT CORRECT !!!!
 * Need to read about move semantics from somewhere.
 *
 * Similar to copying, moving also uses the value of an object to set the value
 * to another object. But, unlike copying, the content is actually transferred
 * from one object (source) to another (destination): the source loses that
 * content, whcih is taken over by the destination. This moving only happens
 * when teh source of hte value is an **unnamed object**. So there are no issues
 * around deep or shallow copies.
 *
 * Move ctor usage scenarios:
 * MyClass create_my_class() {...}
 * MyClass obj = create_my_class()
 *
 * Move assignment operator usage scnearios:
 * MyClass obj2;
 * obj2 = create_my_class();
 */

#include <iostream>
#include <sstream>

unsigned int addrin(void* ptr)
{
    std::stringstream ss;
    ss << ptr;
    std::string hexaddr;
    ss >> hexaddr;
    unsigned int addr = std::stoul(hexaddr, nullptr, 16);
    return addr;
}

template <class T>
class List {
    T* m_list;
    int m_capacity;
    int m_size;

public:
    List(int capacity);
    ~List();
    void append(T element);
    T get(int idx);
    void set(int idx, T element);
    int getSize();
    const std::string& repr();
};

template <class T>
List<T>::List(int capacity)
{
    m_capacity = capacity;
    m_size = 0;
    m_list = new T[m_capacity];
}

template <class T>
List<T>::~List() { delete[] m_list; }

template <class T>
void List<T>::append(T element)
{
    if (m_size == m_capacity) {
        T* newlist = new T[2 * m_capacity];
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

template <class T>
T List<T>::get(int idx)
{
    if (idx < 0 || idx >= m_size)
        throw "illegal index error!";
    return m_list[idx];
}

template <class T>
void List<T>::set(int idx, T element)
{
    if (idx < 0 || idx >= m_size)
        throw "illegal index error!";
    m_list[idx] = element;
}

template <class T>
int List<T>::getSize() { return m_size; }

template <class T>
const std::string& List<T>::repr()
{
    std::string& ret = *(new std::string("<List("));
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

List<int> fibonacci()
{
    List<int> fib { 5 };
    fib.append(1);
    fib.append(1);
    fib.append(2);
    fib.append(3);
    fib.append(5);
    return fib;
}

List<int> pell()
{
    List<int> p { 5 };
    p.append(1);
    p.append(2);
    p.append(5);
    p.append(12);
    p.append(25);
    return p;
}

int main()
{
    // The default move ctor is called
    List<int> seq = fibonacci();
    std::cout << "\nseq(" << std::to_string(addrin(&seq)) << "): " << seq.repr()
              << std::endl;

    // The default move assignment operator is called
    seq = pell();
    std::cout << "\nseq(" << std::to_string(addrin(&seq)) << "): " << seq.repr()
              << std::endl;
}
