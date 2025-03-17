/*
 * Why is uniform initialization better?
 * 1. Forces predictable initial values
 * 2. Prevents narrowing conversions (prevents intialization of int with float
 * and vice versa) 3.
 * 3. Uniform syntax
 */

// clang++ -std=c++20 -Wall -o bin/init Init.cpp

#include <iostream>

class Cookie {
    int calories_;
    std::string flavor_;

public:
    Cookie(int calories, const std::string& flavor)
        : calories_(calories)
        , flavor_(flavor)
    {
    }

    Cookie()
        : calories_(0)
        , flavor_("")
    {
    }

    int calories() const { return calories_; }
    std::string flavor() const { return flavor_; }

    friend std::ostream& operator<<(std::ostream& out, const Cookie& cookie);
};

std::ostream& operator<<(std::ostream& out, const Cookie& cookie)
{
    out << "<Cookie(";
    out << "calories=" << cookie.calories_ << " ";
    out << "flavor=" << cookie.flavor_;
    out << ")>";
    return out;
}

void printArray(int* arr, int len)
{
    for (int i = 0; i < len; i++) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;
}

void oldWay()
{
    // Different ways of initliazing scalars
    // Unitialized - will contain undefined value depending in the platform
    int a1;
    std::cout << a1 << std::endl;

    // Copy initialization
    int a2 = 10;
    std::cout << a2 << std::endl;

    // Direct initialization
    int a3(20);
    std::cout << a3 << std::endl;

    // Different ways of initializing arrays
    // Uninitialized - will contain undefined values depending on the platform
    int arr[3];
    printArray(arr, 3);

    // Copy initialization + aggregate initialization
    int arr2[3] = { 1, 2, 3 };
    printArray(arr2, 3);

    // TODO: What is this type of initalizaiton called?
    int arr3[] = { 1, 2, 3, 4, 5 };
    printArray(arr3, 5);

    // Diffrent ways of initializing objects
    // Call the default ctor
    Cookie c1;
    std::cout << c1 << std::endl;

    // Some sort of copy ctor
    Cookie c2 = Cookie(200, "Chocolate Chip");
    std::cout << c2 << std::endl;

    // Initialize on heap
    Cookie* c3 = new Cookie(180, "Snicker Doodle");
    std::cout << *c3 << std::endl;

    // Narrowing conversions work
    float x;
    std::cout << "Enter float value: " << std::endl;
    std::cin >> x;
    int y = x;
    std::cout << "x = " << x << " y = " << y << std::endl;
}

void newWay()
{
    // Uniform initialization of scalars
    // Value initialization - will initialize to default value
    int a1 {};
    std::cout << a1 << std::endl;

    // Direct initialization
    int a2 { 20 };
    std::cout << a2 << std::endl;

    // Uniform initialization of arrays
    // Each element is initialized with the default type value
    int arr1[3] {};
    printArray(arr1, 3);

    // Also possible to specify a default values for parts of the array
    int arr2[3] { 42 }; // [42, 0, 0]
    printArray(arr2, 3);

    // This will be my preferred way of initializing arrays
    int arr3[] { 1, 2, 3 };
    printArray(arr3, 3);

    // Uniform initialization of objects
    // Default ctor
    Cookie c1 {};
    std::cout << c1 << std::endl;

    // Custom ctor
    Cookie c2 { 180, "Snicker Doodle" };
    std::cout << c2 << std::endl;

    // Also works for heaps
    Cookie* ptr = new Cookie { 200, "Chocolate Chip" };
    std::cout << *ptr << std::endl;

    // Also works for dynamic arrays
    std::cout << "Input lenght of array: " << std::endl;
    int len;
    std::cin >> len;
    int* ptrInts = new int[len] {};
    printArray(ptrInts, len);

    // Narrowing conversions will not work
    float x;
    std::cout << "Enter float value: " << std::endl;
    std::cin >> x;
    // The following will not compile
    // int y{x};
    // std::cout << "x = " << x << " y = " << y << std::endl;
}

int main()
{
    // oldWay();
    newWay();
}