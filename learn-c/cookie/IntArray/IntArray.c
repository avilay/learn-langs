#include "IntArray.h"
#include <stdio.h>
#include <stdlib.h>

char* errorMessageIntArray(IntArrayStatus err)
{
    switch (err) {
    case INTARRAY_OK:
        return "Everything OK";
    case INTARRAY_OOM:
        return "OutOfMemory Exception";
    case INTARRAY_BAD_CAPACITY:
        return "Capacity needs to be between [1, 1000]";
    case INTARRAY_OVERFLOW:
        return "Array is overflowing";
    case INTARRAY_UNDERFLOW:
        return "Array length is 0";
    }
    return "No Error";
}

IntArrayStatus createIntArray(int capacity, IntArray** const intsPtr)
{
    if (capacity < 1 || capacity > 1000) {
        return INTARRAY_BAD_CAPACITY;
    }

    IntArray* newArray = (IntArray*)malloc(sizeof(IntArray));
    if (!newArray) {
        return INTARRAY_OOM;
    }

    newArray->values = (int*)malloc(sizeof(int) * capacity);
    if (!newArray->values) {
        free(newArray);
        return INTARRAY_OOM;
    }

    newArray->capacity = capacity;
    newArray->len = 0;
    *intsPtr = newArray;
    return INTARRAY_OK;
}

IntArrayStatus pushToIntArray(IntArray* ints, int value)
{
    if (ints->capacity == ints->len) {
        ints->values = realloc(ints->values, sizeof(int) * ints->capacity * 2);
        ints->capacity = ints->capacity * 2;
    }
    ints->values[ints->len] = value;
    ints->len += 1;
    return INTARRAY_OK;
}

IntArrayStatus popFromIntArray(IntArray* const ints, int** const poppedValue)
{
    if (ints->len == 0)
        return INTARRAY_UNDERFLOW;

    int value = ints->values[ints->len - 1];
    ints->len -= 1;
    *poppedValue = &value;

    // Without the const in poppedValue, I could've done something like this -
    // int* ptrValue = &value;
    // poppedValue = &ptrValue;
    // which would've given a segmentation fault.

    return INTARRAY_OK;
}

void printIntArray(IntArray const* const ints)
{
    printf("[");
    for (int i = 0; i < ints->len - 1; i++) {
        printf("%d, ", ints->values[i]);
    }
    printf("%d]\n", ints->values[ints->len - 1]);
}