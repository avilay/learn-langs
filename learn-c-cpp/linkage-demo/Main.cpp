#include <format>
#include <iostream>

using namespace std;

// Just do a forward declaration of the global variable that I want to use
extern int gDefaultCalories;

// Trying to forward declare a const global will work, but using it will give linker error.
// extern const string gDefaultFlavor;

// Forward declare an const global that was defined with extern keyword
extern const int gServingSize;

// Trying to forward declare a static function will work but invoking it will give linker error.
// void helper();

// Trying to forward declare a static variable will work, but using it will give linker error.
// extern int helpValue;

// A forward declaration of a function does not require the extern keyword
void bake(int calories, string flavor = "");

int main()
{
    cout << format("Default calories = {}", gDefaultCalories) << endl;

    // This will give a link-time error
    // cout << format("Default flavor = {}", gDefaultFlavor) << endl;
    // helper();
    cout << format("Help value is {}", helpValue) << endl;

    cout << format("Serving size = {}", gServingSize) << endl;

    bake(gDefaultCalories);
}