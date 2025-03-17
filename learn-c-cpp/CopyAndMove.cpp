// clang++ -std=c++20 -Wall -fno-elide-constructors -o bin/copyandmove CopyAndMove.cpp

#include <format>
#include <iostream>

class Cookie {
    int calories;
    std::string flavor;

public:
    // Default ctor
    Cookie()
        : calories(0)
        , flavor("")
    {
        std::cout << "Cookie()" << std::endl;
    }

    // Parameterized ctor
    Cookie(int calories, const std::string& flavor)
        : calories(calories)
        , flavor(flavor)
    {
        std::cout << "Cookie(int, const std::string&)" << std::endl;
    }

    // Copy ctor
    Cookie(const Cookie& copy)
        : calories(copy.calories)
        , flavor(copy.flavor)
    {
        std::cout << "Cookie(const Cookie&)" << std::endl;
    }

    // Move ctor
    Cookie(Cookie&& move) noexcept
        : calories(move.calories)
        , flavor(move.flavor)
    {
        std::cout << "Cookie(Cookie&&)" << std::endl;
    }

    // Copy assignment
    Cookie& operator=(const Cookie& other)
    {
        std::cout << "Cookie& operator=(const Cookie&)" << std::endl;
        if (this == &other) {
            return *this;
        }

        calories = other.calories;
        flavor = other.flavor;
        return *this;
    }

    // Move assignment
    Cookie& operator=(Cookie&& other) noexcept
    {
        std::cout << "Cookie& operator=(Cookie&&)" << std::endl;
        if (this == &other) {
            return *this;
        }

        calories = other.calories;
        flavor = other.flavor;
        return *this;
    }

    int getCalories()
    {
        return calories;
    }

    std::string getFlavor()
    {
        return flavor;
    }
};

void bake(Cookie cookie)
{
    std::cout << std::format("Baking {} cookie with {} calories", cookie.getFlavor(), cookie.getCalories()) << std::endl;
}

Cookie bakeChocolateChip()
{
    Cookie cookie { 200, "Chocolate Chip" };
    return cookie;
}

void copyDemo()
{
    /* Copy ctor */

    Cookie c1;
    // Output -
    // Cookie()

    Cookie c2 { c1 };
    // Output -
    // Cookie(const Cookie&)

    Cookie c3 = c1;
    // Ouptut -
    // Cookie(const Cookie&)

    bake(c1);
    // Output -
    // Cookie(const Cookie&)
    // Baking  cookie with 0 calories

    /* Copy assignment */

    Cookie c4 { 200, "Snicker Doodle" };
    c3 = c4;
    // Output -
    // Cookie(int, const std::string&) to create c4
    // Cookie& operator=(const Cookie&) when c4 is assigned to c3
}

void moveDemo()
{
    /* Move ctor */

    Cookie c4 { bakeChocolateChip() };
    // Output -
    // Cookie(int, const std::string&) by bakeChocolateChip
    // Cookie(Cookie&&) when bakeChocolateChip returns cookie

    Cookie c5 = bakeChocolateChip();
    // Output -
    // Cookie(int, const std::string&) by bakeChocolateChip
    // Cookie(Cookie&&) when bakeChocolateChip returns cookie

    /* Move assignment */

    Cookie c6;
    c6 = bakeChocolateChip();
    // Output -
    // Cookie() to create c6
    // Cookie(int, const std::string&) by bakeChocolateChip
    // Cookie(Cookie&&) when bakeChocolateChip returns cookie
    // Cookie& operator=(Cookie&&) when the temp Cookie is assigned to c6
}

int main()
{
    // moveDemo();
    // copyDemo();
    bake(bakeChocolateChip());
}