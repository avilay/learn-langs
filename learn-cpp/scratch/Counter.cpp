#include "Counter.h"

Counter::Counter()
    : counter(0)
{
}

Counter::Counter(int initialValue)
    : counter(initialValue)
{
}

void Counter::increment() { counter += 1; }
void Counter::decrement() { counter -= 1; }
int Counter::value() const { return counter; }