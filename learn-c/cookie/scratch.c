#include "scratch.h"
#include <stdio.h>
#include <stdlib.h>

ScratchStatus createCookie(int calories, char* flavor, Cookie** cookiePtr)
{
    Cookie* newCookie = (Cookie*)malloc(sizeof(Cookie));
    if (!newCookie) {
        return SCRATCH_NOTOK;
    }
    newCookie->calories = calories;
    newCookie->flavor = flavor;
    *cookiePtr = newCookie;
    return SCRATCH_OK;
}
