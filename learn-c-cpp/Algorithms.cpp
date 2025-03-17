// clang++ -std=c++20 -Wall -o bin/scratch Algorithms.cpp && bin/scratch
#include <algorithm>
#include <format>
#include <iostream>
#include <vector>

struct Cookie {
    int calories;
    std::string flavor;

    std::string repr() const
    {
        return std::format("<Cookie(calories={}, flavor={})>", calories, flavor);
    }
};

void sortingDemo(std::vector<Cookie>& cookies)
{
    sort(
        cookies.begin(),
        cookies.end(),
        [](const auto& x, const auto& y) { return x.calories < y.calories; });

    for (const auto& cookie : cookies) {
        std::cout << cookie.repr() << std::endl;
    }
}

void countingDemo(const std::vector<Cookie>& cookies)
{
    auto count {
        std::count_if(
            cookies.begin(),
            cookies.end(),
            [](const auto& x) { return x.calories < 200; })
    };
    std::cout << count << std::endl;
}

void findIfDemo(const std::vector<Cookie>& cookies)
{
    auto it { std::find_if(
        cookies.begin(),
        cookies.end(),
        [](const auto& x) { return x.calories == 200; }) };

    if (it != cookies.end()) {
        std::cout << it->repr() << std::endl;
    }
}

void forEachDemo(const std::vector<Cookie>& cookies)
{
    std::for_each(
        cookies.begin(),
        cookies.end(),
        [](const auto& x) { std::cout << x.repr() << std::endl; });
}

int main()
{
    std::vector<Cookie> cookies {};
    cookies.push_back(Cookie { 200, "Chocolate Chip" });
    cookies.push_back(Cookie { 180, "Snicker Doodle" });
    cookies.push_back(Cookie { 220, "Snicker Doodle" });
    cookies.push_back(Cookie { 240, "Double Chocolate Chip" });
    cookies.push_back(Cookie { 170, "Semi Sweet Chocolate" });

    // sortingDemo(cookies);
    // countingDemo(cookies);
    // findIfDemo(cookies);
    forEachDemo(cookies);
}