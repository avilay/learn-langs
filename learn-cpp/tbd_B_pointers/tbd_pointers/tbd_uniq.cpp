/*
 * unique_ptr features -
 * p.get() gets the underlying pointer
 * Explicit ctor so assignment will not work
 * Call reset to clear the underlying pointer and reassign
 * Cannot reassign to another pointer
 * Cannot be copied - so cannot be passed by value to a function
 *  Use of std::move
 * Does not need to be deleted and assigned to nullptr like raw pointers
 */

#include <iostream>
#include <memory>

class CookieJar {
  int quantity_;
  std::string flavor_;

 public:
  CookieJar() : quantity_(0), flavor_("") {
    std::cout << "CookieJar::CookieJar()" << std::endl;
  }
  CookieJar(int quantity, std::string flavor)
      : quantity_(quantity), flavor_(flavor) {
    std::cout << "CookieJar::CookieJar(qty, flvr)" << std::endl;
  }
  int getQuantity() { return quantity_; }
  std::string getFlavor() { return flavor_; }
  void add() { quantity_ += 1; }
  ~CookieJar() { std::cout << "CookieJar::~CookieJar()" << std::endl; }
};

CookieJar* getCookieJar() {
  CookieJar* jar = new CookieJar{100, "Chocolate Chip"};
  return jar;
}

void display(CookieJar* jar) {
  std::cout << "There are " << jar->getQuantity() << " " << jar->getFlavor()
            << " cookies in the cookie jar" << std::endl;
}

void rawPtr() {
  CookieJar* jar = getCookieJar();
  if (jar == nullptr) {
    jar = new CookieJar{};
  }
  jar->add();
  display(jar);
  delete jar;
  jar = nullptr;

  jar = new CookieJar{10, "Snicker Doodle"};
  display(jar);
  delete jar;
}

void inspect(std::unique_ptr<CookieJar> jar) {
  // To call this function, the unique pointer will have to be moved to the
  // by-value arg
  if (jar->getQuantity() > 100) {
    std::cout << "That is too many cookies!" << std::endl;
  } else {
    std::cout << "Good! You are not being greedy." << std::endl;
  }
}

void betterInspect(const std::unique_ptr<CookieJar>& jar) {
  // This function can be called without any gymnastics
  if (jar->getQuantity() > 100) {
    std::cout << "That is too many cookies!" << std::endl;
  } else {
    std::cout << "Good! You are not being greedy." << std::endl;
  }
}

void uniqPtr() {
  // Construct unique pointer given a raw pointer
  std::unique_ptr<CookieJar> jar{getCookieJar()};
  if (jar == nullptr) {
    // This check still works because == is overloaded
    CookieJar* ptr = new CookieJar{};
    jar.reset(ptr);
    // A unique pointer cannot be reassigned, it can only be reset
  }
  jar->add();          // Also works beause operator-> has been overridden
  display(jar.get());  // jar.get() gets the underlying raw pointer

  // No need to delete it and set it to nullptr for safety
  // Just reset it with a new pointer
  jar.reset(new CookieJar{10, "Snicker Doodle"});
  display(jar.get());

  // A better option is to pass by ref
  betterInspect(jar);

  // It is ok to move the pointer as long as we are not going to use the jar
  // variable again. Otherwise using it again will cause a seg fault
  inspect(std::move(jar));

  // No need to delete it.
}

int main() {
  // rawPtr();
  uniqPtr();
}
