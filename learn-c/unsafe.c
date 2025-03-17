/*
This is why C is considered so unsafe. The following code works! It has the
potential of corrupting some other program's memory.

clang -Wall -g -o bin/unsafe unsafe.c
./bin/unsafe
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    int* ints = (int*)malloc(sizeof(int) * 3);

    // Error here where I am assigning 5 elements when I had allocated only 3!
    for (int i = 0; i < 5; i++) {
        printf("ints[%d] = %d\n", i, ints[i]);
    }

    // Another error here where I am trying to read from the 100th element!
    printf("ints[100] = %d\n", ints[100]);

    // Here I am actually able to assign a value to the 100th element
    ints[0] = 0;
    ints[1] = 1;
    ints[2] = 2;
    ints[100] = 3;

    // When I read it back, who knows what I'll get, but I'll get **something**
    // back.
    printf("ints[100] = %d\n", ints[100]);

    return 0;
}