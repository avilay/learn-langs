// clang++ -std=c++20 -Wall -fno-elide-constructors -o bin/cpmv CopyAndMoveExample.cpp

#include <format>
#include <iostream>
#include <sstream>

unsigned int addrin(void* ptr)
{
    std::stringstream ss;
    ss << ptr;
    std::string hexaddr;
    ss >> hexaddr;
    unsigned int addr = stoul(hexaddr, nullptr, 16);
    return addr;
}

template <class T>
class AutoPtr {
    T* ptr;
    int myAddr;

public:
    AutoPtr(T* ptr = nullptr)
        : ptr(ptr)
        , myAddr(0)
    {
        myAddr = addrin(this);
        std::cout << std::format("AutoPtr(T*)[addr={}]", myAddr) << std::endl;
    }

    AutoPtr(const AutoPtr& copy)
        : ptr(nullptr)
        , myAddr(0)
    {
        ptr = new T;
        *ptr = *copy.ptr;
        myAddr = addrin(this);
        std::cout << std::format("AutoPtr(const AutoPtr&)[addr={}]", myAddr) << std::endl;
    }

    AutoPtr(AutoPtr&& rref) noexcept
        : ptr(rref.ptr)
        , myAddr(0)
    {
        rref.ptr = nullptr;
        myAddr = addrin(this);
        std::cout << std::format("AutoPtr(AutoPtr&&)[addr={}]", myAddr) << std::endl;
    }

    AutoPtr& operator=(const AutoPtr& copy)
    {
        std::cout << std::format("AutoPtr& operator=(const AutoPtr&)[addr={}]", myAddr) << std::endl;
        if (this == &copy) {
            return *this;
        }

        delete ptr;

        ptr = new T;
        *ptr = *copy.ptr;
        return *this;
    }

    AutoPtr& operator=(AutoPtr&& rref) noexcept
    {
        std::cout << std::format("AutoPtr& operator=(AutoPtr&&)[addr={}]", myAddr) << std::endl;
        if (this == &rref) {
            return *this;
        }

        ptr = rref.ptr;
        rref.ptr = nullptr;

        return *this;
    }

    ~AutoPtr()
    {
        std::cout << std::format("~AutoPtr[addr={}]", myAddr) << std::endl;
        delete ptr;
    }

    T* operator->()
    {
        return ptr;
    }

    T& operator*()
    {
        return *ptr;
    }
};

class Circle {
public:
    Circle()
        : radius(0.0)
    {
        std::cout << "Circle()" << std::endl;
    }

    Circle(float radius)
        : radius(radius)
    {
        std::cout << "Circle(float)" << std::endl;
    }

    float getRadius()
    {
        return radius;
    }

    ~Circle()
    {
        std::cout << "~Circle" << std::endl;
    }

private:
    float radius;
};

AutoPtr<Circle> makeUnit()
{
    AutoPtr aptr { new Circle { 1.0 } };
    return aptr;
}

float calcArea(AutoPtr<Circle> aptr)
{
    return 3.14 * aptr->getRadius() * aptr->getRadius();
}

int main()
{
    auto unit = makeUnit();
    // Output -
    // Circle(float)
    // AutoPtr(T*)[addr=1863807240]
    // AutoPtr(AutoPtr&&)[addr=1863807344]
    // ~AutoPtr[addr=1863807240]

    float area = calcArea(unit);
    // Output -
    // Circle()
    // AutoPtr(const AutoPtr&)[addr=1863807320]
    // ~AutoPtr[addr=1863807320]
    // ~Circle

    std::cout << area << std::endl;
    // 3.14

    // ~AutoPtr[addr=1863807344]
    // ~Circle
}