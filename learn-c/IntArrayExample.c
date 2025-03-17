#include "IntArray/IntArray.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void die(IntArrayStatus err, char* msg)
{
    printf("FATAL: Error code %s.\n", errorMessageIntArray(err));
    printf("%s\n", msg);
    exit(EXIT_FAILURE);
}

int main(int argc, char* argv[])
{
    IntArray* ints = NULL;
    IntArrayStatus ret;

    if ((ret = createIntArray(3, &ints)) != INTARRAY_OK) {
        die(ret, "Unable to create ints!");
    }

    for (int i = 0; i < 5; i++) {
        if ((ret = pushToIntArray(ints, 10 * i)) != INTARRAY_OK) {
            die(ret, "Unable to push value!");
        }
    }
    printIntArray(ints);

    int* val = NULL;
    int len = ints->len;
    for (int i = 0; i < len + 1; i++) {
        if ((ret = popFromIntArray(ints, &val)) != INTARRAY_OK) {
            die(ret, "Unable to pop value!");
        }
        printf("Popped value: %d\n", *val);
    }

    return EXIT_SUCCESS;
}