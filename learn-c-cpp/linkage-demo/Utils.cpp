#include <format>
#include <iostream>
#include <string>

using namespace std;

// non-const global variables have external linkage by default
int gDefaultCalories { 200 };

// const global variables only have internal linkage by default
const std::string gDefaultFlavor { "Chocolate Chip" };

// but I can make it have external linkage by using the extern keyword
const extern int gServingSize { 2 };

// and I can make non-const global variables have internal linkage by using the
// static keyword
static int helpValue { 10 };

// Even though functions have external linkage by default, I can make them have
// internal linkage by using the static keyword
static void helper()
{
    cout << format("Inside helper using {}", helpValue) << endl;
}

// Functions have external linkage by default
void bake(int calories = 0, string flavor = "")
{
    if (calories == 0) {
        calories = gDefaultCalories;
    }
    if (flavor == "") {
        flavor = gDefaultFlavor;
    }
    helper();
    cout << format("Baking {} cookie with calories {}", flavor, calories) << endl;
}
