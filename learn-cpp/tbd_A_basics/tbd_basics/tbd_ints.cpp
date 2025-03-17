#include <iomanip>
#include <iostream>

using namespace std;

int main()
{
    int n1 { 9241 };
    cout << "n1 = " << showbase << n1 << endl;
    cout << "n1 = " << showbase << hex << n1 << endl;
    cout << "n1 = " << showbase << oct << n1 << endl;
    resetiosflags(ios_base::showbase);

    int n2 { 0x2419 };
    cout << "n2 = " << showbase << n2 << endl;
    cout << "n2 = " << showbase << hex << n2 << endl;
    cout << "n2 = " << showbase << oct << n2 << endl;
    resetiosflags(ios_base::showbase);

    int n3 = { 0b001001000011001 };
    cout << "n3 = " << n3 << endl; // will show up in oct
    cout << "n3 = " << showbase << n3 << endl; // will show up in oct
    cout << "n3 = " << showbase << hex << n3 << endl;
    cout << "n3 = " << showbase << oct << n3 << endl;
    resetiosflags(ios_base::showbase);

    // base 16: 0-9 and A-F
    // int a_apaec { 0x24CD };

    // base  2: 0-1
    // int m_gargan { 0b10010000011001 };

    // base  8: 0-7 - Leading with a 0 not the letter o.
    // int b_reilly { 022031 };
}