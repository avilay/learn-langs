#include "Utils.h"
#include <functional>
#include <iostream>
#include <random>

using namespace std;

vector<float>* randomVec(int len)
{
    cout << "DEBUG: Generating random vector with " << len << " elements." << endl;
    vector<float>* v = new vector<float> {};
    default_random_engine generator;
    uniform_real_distribution<float> distribution(-1.0, 1.0);
    auto generate = bind(distribution, generator);
    for (int i = 0; i < len; i++) {
        v->push_back(generate());
    }
    return v;
}

vector<float>* zeros(int len)
{
    cout << "DEBUG: Generating zero vector with " << len << " elements." << endl;
    vector<float>* v = new vector<float> {};
    for (int i = 0; i < len; i++) {
        v->push_back(0.0);
    }
    return v;
}

void add(const vector<float>& x, const vector<float>& y, vector<float>& z)
{
    auto itx = x.begin();
    auto ity = y.begin();
    auto itz = z.begin();
    while (itx != x.end() and ity != y.end() and itz != z.end()) {
        *itz = *itx + *ity;
        itx += 1;
        ity += 1;
        itz += 1;
    }
}

int main()
{
    string msg { "Enter vector length:" };
    int len = inputInt(msg);

    auto x = randomVec(len);
    auto y = randomVec(len);
    auto z = zeros(len);

    add(*x, *y, *z);

    cout << "x = " << *x << endl;
    cout << "y = " << *y << endl;
    cout << "z = " << *z << endl;

    delete x;
    delete y;
    delete z;
}