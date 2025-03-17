#include <iostream>

class NotDerived;

class Base {
  int m_cookie;

public:
  Base(int cookie) : m_cookie(cookie) {}
  int cookie() const { return m_cookie; }
  virtual void dummy() { std::cout << "Base::dummy" << std::endl; }
  operator NotDerived();
};

class Derived : public Base {
  int m_advanced_cookie;

public:
  Derived(int advanced_cookie)
      : m_advanced_cookie(advanced_cookie), Base(advanced_cookie / 10) {}
};

class NotDerived {
  int m_super_advanced_cookie;

public:
  NotDerived(int super_advanced_cookie)
      : m_super_advanced_cookie(super_advanced_cookie) {}
  NotDerived(const Base &base) { m_super_advanced_cookie = base.cookie() * 10; }
};

Base::operator NotDerived() { return NotDerived(m_cookie / 10); }

void withDerived() {
  Derived *der1 = new Derived(100);
  Base *base;

  // Upcasting is safe and is allowed by default
  base = der1;

  // Downcasting is not allowed by default
  // Derived *der2 = base;

  // Of course I can do an explicit downcast
  // but that can be potentially unsafe (not in this case)
  Derived *der2 = (Derived *)base;

  // best to use dynamic_cast
  Derived *der3 = dynamic_cast<Derived *>(base);
  if (der3 == 0) {
    std::cout << "It was not safe to cast base* to Derived*!" << std::endl;
  } else {
    std::cout << "base* successfully casted to Derived*" << std::endl;
  }
}

void withNotDerived() {
  Base *base = new Base(10);

  // By default this will not work
  // NotDerived *nder = base;

  // this will work because of the conversion ctor
  NotDerived nder1{*base};

  // this will also work because of the conversion ctor
  NotDerived *nder3 = new NotDerived(*base);

  // this is going to fail - even though I have overridden the typecast operator
  // in Base
  NotDerived *nder2 = dynamic_cast<NotDerived *>(base);
  if (nder2 == 0) {
    std::cout << "It was not safe to cast base* to NotDerived*!" << std::endl;
  } else {
    std::cout << "base* successfully casted to Derived*" << std::endl;
  }
}

void withVoid() {
  Base *base = new Base(100);
  void *tmp = base;

  // this will not compile
  // Base *base_cpy = dynamic_cast<Base *>(tmp);
}

int main(int argc, char **argv) {
  withDerived();
  // withNotDerived();
  // withVoid();
}
