#include "Cookie.h"
#include "Utils.h"

Cookie::Cookie()
    : calories(0)
    , flavor("")

{
    auto addr = addrin(this);
    cout << "Cookie() at " << addr << endl;
}

Cookie::Cookie(int calories, string flavor)
    : calories(calories)
    , flavor(flavor)
{
    auto addr = addrin(this);
    cout << "Cookie(int, string) at " << addr << endl;
}

Cookie::Cookie(Cookie& copy)
    : calories(copy.calories)
    , flavor(copy.flavor)
{
    auto addr = addrin(this);
    cout << "Cookie(Cookie&) at " << addr << endl;
}

Cookie::Cookie(Cookie&& anon)
    : calories(anon.calories)
    , flavor(anon.flavor)
{
    auto addr = addrin(this);
    cout << "Cookie(Cookie&&) at" << addr << endl;
}

Cookie::~Cookie()
{
    auto addr = addrin(this);
    cout << "~Cookie() at " << addr << endl;
}

Cookie& Cookie::operator=(Cookie& rhs)
{
    this->calories = rhs.calories;
    this->flavor = rhs.flavor;
    cout << "operator=(Cookie&)" << endl;
    return *this;
}

Cookie& Cookie::operator=(Cookie&& anon)
{
    this->calories = anon.calories;
    this->flavor = anon.flavor;
    cout << "operator=(Cookie&&)" << endl;
    return *this;
}

int Cookie::getCalories() const
{
    return calories;
}

string Cookie::getFlavor() const
{
    return flavor;
}

std::ostream&
operator<<(std::ostream& out, const Cookie& cookie)
{
    out << "<Cookie(";
    out << "calories=" << cookie.getCalories() << " ";
    out << "flavor=" << cookie.getFlavor();
    out << ")>";
    return out;
}