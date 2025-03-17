#pragma once

class Counter {
    int counter;

public:
    Counter();
    Counter(int);
    void increment();
    void decrement();
    int value() const;
};
