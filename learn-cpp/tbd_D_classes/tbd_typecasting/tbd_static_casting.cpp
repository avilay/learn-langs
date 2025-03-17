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
  int advancedCookie() const { return m_advanced_cookie; }
};

class NotDerived {
  int m_super_advanced_cookie;
  std::string m_name;
  float m_id;

public:
  NotDerived(int super_advanced_cookie)
      : m_super_advanced_cookie(super_advanced_cookie) {
    m_name = "Super Advanced";
    m_id = 3.141;
  }
  int superAdvancedCookie() const { return m_super_advanced_cookie; }
  std::string name() const { return m_name; }
  float id() const { return m_id; }
};

Base::operator NotDerived() { return NotDerived(m_cookie / 10); }

void withNotDerived() {
  Base *base = new Base(10);

  // will not compile
  // NotDerived *nder = static_cast<NotDerived *>(base);
}

void withVoid() {
  NotDerived *nder = new NotDerived(100);
  void *tmp = nder;

  NotDerived *nder_cpy = static_cast<NotDerived *>(tmp);
  std::cout << nder_cpy->superAdvancedCookie() << std::endl;

  Derived *der = static_cast<Derived *>(tmp);
  std::cout << der->advancedCookie() << std::endl;

  Derived *der2 = (Derived *)(tmp);
  std::cout << der2->advancedCookie() << std::endl;
}

int main() { withVoid(); }