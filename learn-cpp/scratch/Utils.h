#pragma once

#include <iostream>
// #include <sstream>
#include <vector>

unsigned int addrin(void*);

// See https://isocpp.org/wiki/faq/templates#templates-defn-vs-decl for
// why template functions have to be defined in the header file
template <typename T>
std::ostream& operator<<(std::ostream& out, std::vector<T>& arr)
{
    out << "[ ";
    for (auto x : arr) {
        out << x << " ";
    }
    out << "]";
    return out;
}

std::ostream& operator<<(std::ostream&, std::pair<float*, int>);

int inputInt(std::string&);