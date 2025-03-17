// clang++ -std=c++20 -Wall -o bin/scratch Scratch.cpp
#include <format>
#include <iostream>
#include <typeinfo>

int main()
{
    // Dynamic arrays are arrays whose length is not known at compile time.
    // These have to be initialized on the heap
    int len;
    std::cout << "Enter length of array: " << std::endl;
    std::cin >> len;
    // The following will not compile
    // int nums2[len]{};
    int* ptr = new int[len];
    for (int i = 0; i < len; i++) {
        std::cout << ptr[i] << " ";
    }
    std::cout << std::endl;
}