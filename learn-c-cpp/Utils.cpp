#include <format>
#include <iostream>
#include <sstream>

std::string inputStr()
{
    std::string name {};
    std::getline(std::cin, name);
    return name;
}

unsigned int addrin(void* ptr)
{
    std::stringstream ss;
    ss << ptr;
    std::string hexaddr;
    ss >> hexaddr;
    unsigned int addr = stoul(hexaddr, nullptr, 16);
    return addr;
}

int inputInt(const std::string& msg)
{
    int n;
    std::cout << msg << " " << std::endl;
    std::cin >> n;
    return n;
}

class Circle {
private:
    float radius;

public:
    Circle(float radius)
        : radius(radius)
    {
        std::cout << "Circle(float)" << std::endl;
    }

    Circle(const Circle& copy)
        : radius(copy.radius)
    {
        std::cout << "Circle(Circle&)" << std::endl;
    }

    Circle(Circle&& move)
        : radius(move.radius)
    {
        std::cout << "Circle(Circle&&)" << std::endl;
    }

    float getRaidus() const
    {
        return radius;
    }
};

class Cookie {
private:
    int calories;
    std::string flavor;

public:
    Cookie(int calories, const std::string& flavor)
        : calories(calories)
        , flavor(flavor)
    {
        std::cout << "Cookie(int, const std::string&)" << std::endl;
    }

    Cookie(const Cookie& copy)
        : calories(copy.calories)
        , flavor(copy.flavor)
    {
        std::cout << "Cookie(const Cookie&)" << std::endl;
    }

    Cookie& operator=(const Cookie& other) = default;

    Cookie(Cookie&& move) noexcept
        : calories(move.calories)
        , flavor(move.flavor)
    {
        std::cout << "Cookie(Cookie&&)" << std::endl;
    }

    Cookie& operator=(Cookie&& other) noexcept = default;

    std::string repr()
    {
        return std::format("<Cookie(calories={}, flavor={})>", calories, flavor);
    }
};
