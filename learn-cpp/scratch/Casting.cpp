#include "Casting.h"
#include <math.h>

RationalNumber::RationalNumber(double value)
    : value(value)
{
}

double RationalNumber::getValue() { return value; }

IrrationalNumber::IrrationalNumber(int value, int oneByPower)
    : value(value)
    , power((float)1 / oneByPower)
{
}

double IrrationalNumber::getValue()
{
    return pow(value, power);
}

IrrationalNumber::operator RationalNumber()
{
    return RationalNumber { getValue() };
}

IrrationalNumber::operator RationalNumber*()
{
    return new RationalNumber { getValue() };
}

Integer::Integer(int val)
    : RationalNumber((double)val)
    , value(val)
{
}

double Integer::getValue() { return (double)value; }
// int Integer::getIntValue() { return value; }