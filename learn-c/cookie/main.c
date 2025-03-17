#include "IntArray/IntArray.h"
#include "scratch.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    IntArray* ints = NULL;
    IntArrayStatus ret;
    if ((ret = createIntArray(5, &ints)) != INTARRAY_OK) {
        printf("Got ERROR!");
        printf(errorMessageIntArray(ret));
        return EXIT_FAILURE;
    }

    Cookie* cookie = NULL;
    ScratchStatus scratchRet;
    if ((scratchRet = createCookie(200, "Chocolate Chip", &cookie)) != SCRATCH_OK) {
        printf("Got ERROR!");
        return EXIT_FAILURE;
    }
    printf("Done\n");
}