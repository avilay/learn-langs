#include <exception>
#include <iostream>

class AppRuntimeError : public std::runtime_error {
public:
  AppRuntimeError(const std::string &message) : std::runtime_error(message) {}
};

class ShouldNeverHappenError : public std::logic_error {
public:
  ShouldNeverHappenError(const std::string &message)
      : std::logic_error(message) {}
};

class MyCustomException : public std::exception {
  std::string m_message;

public:
  MyCustomException(const std::string &message) : m_message(message) {}
  virtual const char *what() const throw() { return m_message.c_str(); }
};

std::string &haikunate(int nonce) {
  if (nonce == 42) {
    throw ShouldNeverHappenError("Got nonce " + std::to_string(nonce));
  }

  if (nonce == -1) {
    throw AppRuntimeError("KA-BOOM!");
  }

  if (nonce < -10) {
    throw MyCustomException("Oops! Got nonce " + std::to_string(nonce));
  }

  return *(new std::string("whispering-palms-" + std::to_string(nonce)));
}

int main(int argc, char **argv) {
  std::cout << "Enter nonce: ";
  int nonce;
  std::cin >> nonce;
  try {
    std::string &haiku = haikunate(nonce);
    std::cout << haiku << std::endl;
  } catch (ShouldNeverHappenError snhe) {
    std::cout << "ShouldNeverHappenError: " << snhe.what() << std::endl;
  } catch (MyCustomException mce) {
    std::cout << "MyCustomException: " << mce.what() << std::endl;
  }
}
