class RationalNumber {
    double value;

public:
    RationalNumber(double);
    double getValue();
};

class Integer : public RationalNumber {
    int value;

public:
    Integer(int);
    double getValue();
    // int getIntValue();
};

class IrrationalNumber {
    int value;
    float power;

public:
    IrrationalNumber(int, int);
    double getValue();
    operator RationalNumber();
    operator RationalNumber*();
};
