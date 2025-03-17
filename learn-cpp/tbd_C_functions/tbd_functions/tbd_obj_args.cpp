// clang++ -std=c++17 -o ~/Desktop/temp/bin/scratch obj_args.cpp

#include <iostream>
#include <sstream>

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

struct Cookie {
    int calories;
    std::string flavor;

    Cookie(int cals, const std::string& flvr)
        : calories(cals)
        , flavor(flvr)
    {
    }
};

std::ostream& operator<<(std::ostream& out, const Cookie& cookie)
{
    out << "<Cookie(";
    out << "calories=" << cookie.calories << " ";
    out << "flavor=" << cookie.flavor;
    out << ")>";
    return out;
}

void passByValue(Cookie cookie)
{
    // Cannot do much with this
    auto addr = addrin(&cookie);
    cout << "passByValue: " << cookie << " is in address " << addr << endl;
}

void passByRef(Cookie& cookie)
{
    auto addr = addrin(&cookie);
    cout << "passByRef: " << cookie << " is in address " << addr << endl;
}

int main()
{
    // chocolateChip :: Cookie
    auto chocolateChip = Cookie { 200, "Chocolate Chip" };
    auto addr = addrin(&chocolateChip);
    cout << "main: " << chocolateChip << " is in address " << addr << endl;
    passByValue(chocolateChip);
    passByRef(chocolateChip);

    // snickerDoodle :: Cookie*
    auto snickerDoodle = new Cookie { 180, "Snicker Doodle" };
    addr = addrin(snickerDoodle);
    cout << "main: " << *snickerDoodle << " is in address " << addr << endl;
    passByValue(*snickerDoodle);
    passByRef(*snickerDoodle);
}