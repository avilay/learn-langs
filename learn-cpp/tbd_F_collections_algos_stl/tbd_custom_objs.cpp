/*
 * To add custom objects to containers, we need to define two predicates
 * (callable classes), one that returns the hash value of the object and another
 * that tests for equality.
 */

#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

class Cookie {
  int m_calories;
  std::string m_flavor;

public:
  Cookie(int calories, const std::string &flavor)
      : m_calories(calories), m_flavor(flavor) {}

  int calories() const { return m_calories; }
  std::string flavor() const { return m_flavor; }

  friend std::ostream &operator<<(std::ostream &out, const Cookie &cookie);
};

std::ostream &operator<<(std::ostream &out, const Cookie &cookie) {
  out << "<Cookie(";
  out << "calories=" << cookie.m_calories << " ";
  out << "flavor=" << cookie.m_flavor;
  out << ")>";
  return out;
}

struct CookieHash {
  size_t operator()(const Cookie &cookie) const {
    auto s1 = std::hash<int>{}(cookie.calories());
    auto s2 = std::hash<std::string>{}(cookie.flavor());
    return s1 ^ s2;
  }
};

struct CookieEquality {
  bool operator()(const Cookie &first, const Cookie &second) const {
    return first.calories() == second.calories() &&
           first.flavor() == second.flavor();
  }
};

void testCallables() {
  auto cc = Cookie{200, "Chocolate Chip"};
  auto hash_fn = CookieHash{};
  std::cout << hash_fn(cc) << std::endl;

  auto cc2 = Cookie{200, "Chocolate Chip"};
  auto equality_fn = CookieEquality{};
  std::cout << equality_fn(cc, cc2) << std::endl;
  auto oatr = Cookie{180, "Oatmeal Raisin"};
  std::cout << equality_fn(oatr, cc) << std::endl;
}

int main(int argc, char **argv) {
  auto cc = Cookie{200, "Chocolate Chip"};
  auto oatr = Cookie{180, "Oatmeal Raisin"};
  auto sd = Cookie{220, "Snicker Doodle"};

  auto arr = std::vector<Cookie>{};
  arr.push_back(cc);
  arr.push_back(oatr);
  arr.push_back(sd);
  std::cout << arr[2] << std::endl;

  auto hsh = std::unordered_map<Cookie, int, CookieHash, CookieEquality>{};
  hsh.insert({cc, 10});
  hsh.insert({oatr, 20});
  hsh.insert({sd, 12});
  std::cout << "Number of snicker doodles: " << hsh.at(sd) << std::endl;

  auto set = std::unordered_set<Cookie, CookieHash, CookieEquality>{};
  set.insert(cc);
  set.insert(sd);
  std::cout << set.count(oatr) << std::endl;
  std::cout << set.count(cc) << std::endl;
}
