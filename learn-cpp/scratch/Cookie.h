#pragma once

#include <iostream>
using namespace std;

class Cookie {
    int calories;
    string flavor;

public:
    Cookie();

    Cookie(int, string);

    // Copy
    Cookie(Cookie&);
    Cookie& operator=(Cookie&);

    // Move
    Cookie(Cookie&&);
    Cookie& operator=(Cookie&&);

    ~Cookie();

    int getCalories() const;
    string getFlavor() const;

    friend std::ostream& operator<<(std::ostream&, const Cookie&);
};
