#pragma once

#include <exception>
#include <string>

class AssertionError : public std::runtime_error {
public:
    AssertionError(const std::string&);
};

void assert(bool, std::string);

std::string strip(std::string text);