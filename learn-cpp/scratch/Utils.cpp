#include "Utils.h"
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

unsigned int addrin(void* ptr)
{
    stringstream ss;
    ss << ptr;
    string hexaddr;
    ss >> hexaddr;
    unsigned int addr = stoul(hexaddr, nullptr, 16);
    return addr;
}

ostream& operator<<(ostream& out, pair<float*, int> array)
{
    float* arr = array.first;
    int len = array.second;
    out << "[";
    for (int i = 0; i < len - 1; i++) {
        out << arr[i] << " ";
    }
    out << arr[len - 1] << "]";
    return out;
}

int inputInt(string& msg)
{
    int n;
    cout << msg << " " << endl;
    cin >> n;
    return n;
}