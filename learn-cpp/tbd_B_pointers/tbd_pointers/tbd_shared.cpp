// clang++ -std=c++17 -o ~/Desktop/temp/bin/shared shared.cpp

/*
 * shared_ptr features -
 * p.get() gets the underlying pointer
 * Explicit ctor so assignment will not work
 * Call reset to clear the underlying pointer and reassign but will not
 *  neccessarily delete the underlying pointer
 * Can be used to pass by-value and copy assignment
 * Does not need to be deleted and assigned to nullptr like raw pointers
 *
 * std::shared_ptr ptr1{cookie_jar};
 *
 * std::shared_ptr ptr2{ptr1};
 *     - OR -
 * std::shared_ptr ptr2{cookie_jar}
 *
 * The above two result in the same memory layout
 *          +-------------+
 * ptr1 --> | cookie_jar |
 *         +-------------+
 *              ^
 *             |
 * ptr2 -------+
 *
 * In the first case when ptr2 goes out of scope, it will not try to delete the
 * underlying object because the ref count was bumped up upon creation of the
 * second pointer. In the second case, ptr2 does not bump up cookie_jar's ref
 * count, so when ptr2 goes out of scope, it will result in cookie_jar getting
 * destroyed resulting in a segfault when ptr1 goes out of scope and trys to
 * destroy an already destroyed object.
 */
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

class Cookie {
    string flavor_;
    int calories_;

public:
    Cookie()
        : calories_(0)
        , flavor_("")
    {
        cout << "Cookie::Cookie()" << endl;
    }

    Cookie(int calories, string flavor)
        : calories_(calories)
        , flavor_(flavor)
    {
        cout << "Cookie::Cookie(string, int)" << endl;
    }

    ~Cookie()
    {
        auto addr = addrin(this);
        cout << "Cookie::~Cookie() for cookie in " << addr << endl;
    }

    int getCalories() const
    {
        return calories_;
    }

    const string getFlavor() const
    {
        return flavor_;
    }

    friend std::ostream& operator<<(std::ostream& out, const Cookie& cookie);
};

std::ostream&
operator<<(std::ostream& out, const Cookie& cookie)
{
    out << "<Cookie(";
    out << "calories=" << cookie.getCalories() << " ";
    out << "flavor=" << cookie.getFlavor();
    out << ")>";
    return out;
}

void passByValue(shared_ptr<Cookie> cookie)
{
    cout << "passByValue: " << *cookie.get() << " has count "
         << cookie.use_count() << endl;
}

void passByRef(shared_ptr<Cookie>& cookie)
{
    cout << "passByRef: " << *cookie.get() << " has count "
         << cookie.use_count() << endl;
}

void demoRefCounting()
{
    auto chocolateChip = new Cookie { 200, "Chocolate Chip" };
    shared_ptr<Cookie> cookie1 { chocolateChip };

    // demoing calling a method on shared_ptr itself (.use_count), getting the
    // underlying pointer (.get), and calling object methods (->getCalories, ->getFlavor)
    auto count = cookie1.use_count();
    auto addr = addrin(cookie1.get());
    auto cals = cookie1->getCalories();
    auto flvr = cookie1->getFlavor();
    cout << "main: cookie1-" << endl;
    cout << "count=" << count << ", addr=" << addr << ", calories=" << cals
         << ", flavor=" << flvr << endl;

    {
        // shared_ptr<Cookie> cookie2 { chocolateChip }; -- bad will result in seg fault.
        shared_ptr<Cookie> cookie2 { cookie1 };
        cout << "main scoped: " << endl
             << "cookie1 count=" << cookie1.use_count() << ", addr="
             << addrin(cookie1.get()) << endl
             << "cookie2 count=" << cookie2.use_count() << ", addr="
             << addrin(cookie2.get()) << endl;
        // cookie2 goes out of scope, if badly constructed, the Cookie dtor will
        // be called here resulting in seg fault later on.
    }

    passByRef(cookie1);
    passByValue(cookie1);

    // cookie1 goes out of scope here.
}

// TODO: Show memory leak
int main()
{
    cout << "Starting demo" << endl
         << endl;
    demoRefCounting();
    cout << endl
         << "Demo ended" << endl;
}