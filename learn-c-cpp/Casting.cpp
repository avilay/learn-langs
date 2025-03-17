// clang++ -std=c++20 -Wall -o bin/scratch Casting.cpp
#include <exception>
#include <format>
#include <iostream>

class Base {
public:
    virtual void baseMethod() const
    {
        std::cout << "Base::baseMethod" << std ::endl;
    }
};

class Derived : public Base {
public:
    void baseMethod() const override
    {
        std::cout << "Derived::baseMethod" << std::endl;
    }

    void derivedMethod() const
    {
        std::cout << "Derived::derivedMethod" << std::endl;
    }
};

void foo(const Base& base)
{
    try {
        const auto obj { dynamic_cast<const Derived&>(base) };
        obj.derivedMethod();
    } catch (std::bad_cast) {
        base.baseMethod();
    }
}

void bar(const Base* base)
{
    const auto obj { dynamic_cast<const Derived*>(base) };
    if (obj) {
        obj->derivedMethod();
    } else {
        base->baseMethod();
    }
}

int main()
{
    Derived derived {};

    foo(derived);
    // Output -
    // Derived::derivedMethod

    bar(&derived);
    // Output -
    // Derived::derivedMethod

    Base& bref { static_cast<Base&>(derived) };
    bref.baseMethod();
    // Output -
    // Derived::baseMethod

    Base base {};

    foo(base);
    // Output -
    // Base::baseMethod

    bar(&base);
    // Output -
    // Base::baseMethod
}