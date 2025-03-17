#include <stdio.h>
#include <stdlib.h>

typedef struct Cookie {
    int calories;
    char* flavor;
} Cookie;

void printCookie(Cookie const* const cookie)
{
    printf("<Cookie(address = %p calories = %d flavor = %s)>\n", cookie, cookie->calories, cookie->flavor);
}

void demoNoConst()
{
    Cookie chocolateChip;
    chocolateChip.calories = 200;
    chocolateChip.flavor = "Chocolate Chip";

    Cookie oatmealRaisin;
    oatmealRaisin.calories = 180;
    oatmealRaisin.flavor = "Oatmeal Raisin";

    printf("\nNo Const Demo:\n");
    Cookie* cookie
        = &chocolateChip;
    printCookie(cookie);

    // OK: Changing the address contents
    *cookie = oatmealRaisin;
    printCookie(cookie);

    // Ok: Pointing to a different address
    cookie = &oatmealRaisin;
    printCookie(cookie);
}

void demoConstPointer()
{
    Cookie chocolateChip;
    chocolateChip.calories = 200;
    chocolateChip.flavor = "Chocolate Chip";

    Cookie oatmealRaisin;
    oatmealRaisin.calories = 180;
    oatmealRaisin.flavor = "Oatmeal Raisin";

    printf("\nConstant Pointers Demo:\n");

    Cookie* const constPtr = &chocolateChip;
    printCookie(constPtr);

    // Ok: Changing the address contents
    *constPtr = oatmealRaisin;
    printCookie(constPtr);

    // Ok: Changing the object
    constPtr->calories = 220;
    printCookie(constPtr);

    // Error: Cannot point to a different address
    // constPtr = &chocolateChip;
}

void demoPointerToConst()
{
    Cookie chocolateChip;
    chocolateChip.calories = 200;
    chocolateChip.flavor = "Chocolate Chip";

    Cookie oatmealRaisin;
    oatmealRaisin.calories = 180;
    oatmealRaisin.flavor = "Oatmeal Raisin";

    printf("\nPointer to Const Demo:\n");

    // Cookie const* ptrToConst = &chocolateChip;
    const Cookie* ptrToConst = &chocolateChip;

    printCookie(ptrToConst);

    // Error: Cannot change the address contents
    // *ptrToConst = oatmealRaisin;

    // Error: Cannot change the object
    // ptrToConst->calories = 220;

    // Ok: Pointing to a different address
    ptrToConst = &oatmealRaisin;
    printCookie(ptrToConst);
}

void demoConstPointerToConst()
{
    Cookie chocolateChip;
    chocolateChip.calories = 200;
    chocolateChip.flavor = "Chocolate Chip";

    Cookie oatmealRaisin;
    oatmealRaisin.calories = 180;
    oatmealRaisin.flavor = "Oatmeal Raisin";

    printf("\nConst Pointer to Const Demo:\n");

    // Cookie const* const constPtrToConst = &chocolateChip;
    const Cookie* const constPtrToConst = &chocolateChip;
    printCookie(constPtrToConst);

    // Error: Cannot change the address contents
    // *constPtrToConst = oatmealRaisin;

    // Error: Cannot change the object
    // constPtrToConst->calories = 220;

    // Error: Cannot point to a different address
    // constPtrToConst = &oatmealRaisin;
}

int main(int argc, char* argv[])
{
    demoNoConst();
    demoConstPointer();
    demoPointerToConst();
    demoConstPointerToConst();
    return EXIT_SUCCESS;
}