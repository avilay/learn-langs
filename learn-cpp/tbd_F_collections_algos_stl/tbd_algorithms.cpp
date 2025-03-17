#include <algorithm>
#include <iostream>
#include <vector>

class Cookie {
  int m_calories;
  std::string m_flavor;

public:
  Cookie(int calories, const std::string &flavor)
      : m_calories(calories), m_flavor(flavor) {}
  int calories() const { return m_calories; }
  std::string flavor() const { return m_flavor; }
  friend std::ostream &operator<<(std::ostream &out, Cookie &cookie);
};

std::ostream &operator<<(std::ostream &out, Cookie &cookie) {
  out << "<Cookie(";
  out << "calories=" << cookie.m_calories << " ";
  out << "flavor=" << cookie.m_flavor;
  out << ")>";
  return out;
}

std::ostream &operator<<(std::ostream &out, std::vector<Cookie> cookies) {
  out << "[ ";
  for (auto cookie : cookies) {
    out << cookie << " ";
  }
  out << "]";
  return out;
}

void sorting() {
  auto cc = Cookie{200, "Chocolate Chip"};
  auto oatr = Cookie{180, "Oatmeal Raisin"};
  auto sd = Cookie{220, "Snicker Doodle"};

  auto arr = std::vector<Cookie>{cc, oatr, sd};
  std::cout << "Before sorting: " << arr << std::endl;

  // Sort by descending order of calories, i.e., cookie with less calories
  // should show up first. This will perform in-place sorting.
  sort(arr.begin(), arr.end(), [](const auto &first, const auto &second) {
    return first.calories() < second.calories();
  });
  std::cout << "After sorting: " << arr << std::endl;
}

void counting() {
  auto cc = Cookie{200, "Chocolate Chip"};
  auto oatr = Cookie{180, "Oatmeal Raisin"};
  auto sd = Cookie{220, "Snicker Doodle"};
  auto dcc = Cookie{240, "Double Chocolate Chip"};
  auto arr = std::vector<Cookie>{cc, oatr, sd, dcc};

  // Lets count the number of coookies that have >200 cals
  int count = std::count_if(arr.begin(), arr.end(),
                            [](const auto &c) { return c.calories() > 200; });
  std::cout << "Count of cookies with > 200 calories is " << count << std::endl;
}

void finding() {
  auto cc = Cookie{200, "Chocolate Chip"};
  auto oatr = Cookie{180, "Oatmeal Raisin"};
  auto sd = Cookie{220, "Snicker Doodle"};
  auto dcc = Cookie{240, "Double Chocolate Chip"};
  auto arr = std::vector<Cookie>{cc, oatr, sd, dcc};

  // Find the first element with a specific word in it
  auto it = std::find_if(arr.begin(), arr.end(), [](const auto &c) {
    return c.flavor().find("Semi") != std::string::npos;
  });
  if (it == arr.end()) {
    std::cout << "Not found" << std::endl;
  } else {
    std::cout << *it << std::endl;
  }
}

struct Calories {
  std::vector<int> cals;

  void operator()(const Cookie &cookie) {
    if (cookie.calories() > 200) {
      cals.push_back(cookie.calories());
    }
  }
};

void fancyFors() {
  auto cc = Cookie{200, "Chocolate Chip"};
  auto oatr = Cookie{180, "Oatmeal Raisin"};
  auto sd = Cookie{220, "Snicker Doodle"};
  auto dcc = Cookie{240, "Double Chocolate Chip"};
  auto arr = std::vector<Cookie>{cc, oatr, sd, dcc};

  std::for_each(arr.begin(), arr.end(),
                [](const auto &c) { std::cout << c.flavor() << std::endl; });

  Calories cals = std::for_each(arr.begin(), arr.end(), Calories{});
}

int main(int argc, char **argv) {
  // sorting();
  // counting();
  // finding();
  fancyFors();
}