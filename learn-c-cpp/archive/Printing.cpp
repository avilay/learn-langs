// clang++ -std=c++20 -Wall -o bin/printing Printing.cpp

#include <iomanip>
#include <iostream>

using namespace std;

void setPrintOptions()
{
    // Instead of setting all the options with <<, I can use a bitmask to set them
    // all at once.
    cout << setprecision(3)
         << setiosflags(ios_base::boolalpha | ios_base::fixed | ios_base::showbase);
}

void resetPrintOptions()
{
    cout << setprecision(6)
         << resetiosflags(ios_base::boolalpha | ios_base::fixed | ios_base::showbase);
}

void print_plain()
{
    bool is_good = true;
    cout << "Boolean without any options: " << is_good << endl;

    double n = 1234.5678901;
    cout << "Number without any options: " << n << endl;
}

void printSimple()
{
    // Setting the format option on cout one time will set it for all subsequent
    // usages.
    bool isGood = true;
    cout << boolalpha << "Is this good? " << isGood << endl;
    cout << "How about now? " << isGood << endl;

    // Good practice to set the opposite option when done.
    cout << noboolalpha;
}

void printNums()
{
    double n = 1234.5678901;
    cout << "Fixed: " << setprecision(3) << fixed << n << endl;
    cout << "Scientific: " << scientific << n << endl;
    cout << setprecision(6) << fixed;
}

int main()
{
    print_plain();

    printSimple();

    printNums();

    setPrintOptions();
    bool isGood = true;
    double n = 1234.56789001;
    int a = 16;
    cout << "Is this good? " << isGood << " n=" << n << " a=" << hex << a << endl;
    cout << " a=" << oct << a << endl;
    // No straightforward way to print binary form
    resetPrintOptions();
    cout << "Is this good? " << isGood << " n=" << n << " a=" << hex << a << endl;
}
