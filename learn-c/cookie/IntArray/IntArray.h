#pragma once

#include <stdlib.h>

typedef enum IntArrayStatus {
    INTARRAY_OK,
    INTARRAY_OOM,
    INTARRAY_BAD_CAPACITY,
    INTARRAY_OVERFLOW,
    INTARRAY_UNDERFLOW,
} IntArrayStatus;

char* errorMessageIntArray(IntArrayStatus err);

typedef struct IntArray {
    size_t capacity;
    size_t len;
    int* values;
} IntArray;

IntArrayStatus createIntArray(int capacity, IntArray** const newArray);
IntArrayStatus pushToIntArray(IntArray* ints, int value);
IntArrayStatus popFromIntArray(IntArray* const ints, int** const poppedValue);
void printIntArray(const IntArray* const ints);
