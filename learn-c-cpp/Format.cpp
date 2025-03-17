// clang++ -std=c++20 -Wall -o bin/scratch Scratch.cpp
// DOES NOT COMPILE ON MAC OR UBUNTU
#include <format>
#include <iostream>

struct Point {
    int x;
    int y;
};

template <>
struct std::formatter<Point> : std::formatter<std::string> {
    auto format(Point p, format_context& ctx) const
    {
        return formatter<string>::format(
            std::format("[{}, {}]", p.x, p.y), ctx);
    }
};

int main()
{
    // std::cout << std::format("{}", Point { 1, 2 }) << std::endl;
    // int v { 10 };
    Point v { 1, 2 };
    auto s = std::format("{}", v);
    std::cout << s << std::endl;
}