#include "LogLine.h"
#include "Utils.h"
#include <iostream>

using namespace std;

int main()
{
    try {
        testLogLine();
        cout << "LogLine tests passed." << endl;
    } catch (AssertionError err) {
        cout << err.what() << endl;
    }
}