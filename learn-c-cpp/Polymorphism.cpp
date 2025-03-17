// clang++ -std=c++20 -Wall -o bin/poly Polymorphism.cpp

#include <format>
#include <iostream>

class Base {
private:
    int basePrivate;

protected:
    int baseProtected;

public:
    int basePublic;

    Base()
        : basePrivate(10)
        , baseProtected(10)
        , basePublic(10)
    {
        std::cout << "Base()" << std::endl;
    }

    virtual void baseMethod1()
    {
        std::cout << "Base::baseMethod1()" << std::endl;
    }

    virtual void baseMethod2()
    {
        std::cout << "Base::baseMethod2()" << std::endl;
    }
};

class DerivedOne : public Base {
public:
    DerivedOne()
    {
        std::cout << "DerivedOne()" << std::endl;
    }

    void derivedOneOp()
    {
        std::cout << "DerivedOne::derivedOneOp()" << std::endl;
        std::cout << std::format("basePrivate = NO ACCESS, baseProtected = {}, basePublic = {}", baseProtected, basePublic) << std::endl;
    }
};

class DerivedTwo : public Base {
public:
    DerivedTwo()
    {
        std::cout << "DerivedTwo()" << std::endl;
    }

    void baseMethod1()
    {
        std::cout << "Overridden: DerivedTwo::baseMethod1()" << std::endl;
    }
};

void simple()
{
    auto basePtr = new Base();
    basePtr->baseMethod1();
    // Output -
    // Base()
    // Base::baseMethod1()

    auto d1Ptr = new DerivedOne();
    // Output -
    // Base()
    // DerivedOne()

    d1Ptr->baseMethod1();
    // Output -
    // Base::baseMethod1()

    d1Ptr->derivedOneOp();
    // Output -
    // DerivedOne::derivedOneOp()
    // basePrivate = NO ACCESS, baseProtected = 10, basePublic = 10

    auto d2Ptr = new DerivedTwo();
    // Output -
    // Base()
    // DerivedTwo()

    d2Ptr->baseMethod1();
    // Output -
    // Overridden: DerivedTwo::baseMethod1()

    d2Ptr->baseMethod2();
    // Output -
    // Base::baseMethod2()
}

void casted()
{
    Base* basePtr = new DerivedTwo();
    // Output -
    // Base()
    // DerivedTwo()

    basePtr->baseMethod1();
    // Output -
    // Overridden: DerivedTwo::baseMethod1()

    // Cannot call derivedTwoOp()

    basePtr->baseMethod2();
    // Output -
    // Base::baseMethod2()

    delete basePtr;

    basePtr = new DerivedOne();
    // Output -
    // Base()
    // DerivedOne()

    basePtr->baseMethod1();
    // Output -
    // Base::baseMethod1()

    basePtr->baseMethod2();
    // Output -
    // Base::baseMethod2()

    // Cannot call derivedOneOp()
}

void foo(Base& base)
{
    base.baseMethod1();
    base.baseMethod2();
}

void noPointers()
{
    DerivedTwo d2 {};
    foo(d2);
}

int main()
{
    // casted();
    // simple();
    noPointers();
}