#include "Utils.h"
#include <exception>
#include <regex>
#include <sstream>
#include <string>

using namespace std;

AssertionError::AssertionError(const std::string& message)
    : std::runtime_error(message)
{
}

void assert(bool condition, string failureMsg)
{
    if (!condition) {
        throw AssertionError(failureMsg);
    }
}

string strip(string text)
{
    regex leadingSpace { "^\\s+" };
    regex trailingSpace { "\\s+$" };

    stringstream buf1;
    regex_replace(ostreambuf_iterator<char>(buf1), text.begin(), text.end(), leadingSpace, "");
    string text2 = buf1.str();

    stringstream buf2;
    regex_replace(ostreambuf_iterator<char>(buf2), text2.begin(), text2.end(), trailingSpace, "");
    return buf2.str();
}
