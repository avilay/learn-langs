// clang++ -std=c++20 -Wall -o ~/Desktop/temp/bin/simple simple.cpp

#include <iostream>
#include <vector>

using namespace std;

template <typename T>
ostream& operator<<(ostream& out, vector<T> arr)
{
    out << "[ ";
    for (auto x : arr) {
        out << x << " ";
    }
    out << "]";
    return out;
}

int main()
{
    vector<int> ints { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    cout << "ints = " << ints << endl;

    vector<float> floats { 0.0, 1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9 };
    cout << "float = " << floats << endl;
}