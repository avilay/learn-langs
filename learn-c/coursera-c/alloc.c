//
// Created by avilay on 10/13/25.
// This is a simple LIFO memory allocator. The memory must be freed in the reverse order it was allocated in.
//
#include "alloc.h"
#include <stddef.h>

#define CAPACITY 1024

// Good way to maintain module state is to use static variables
// These are global in lifetime, but not visible outside this module.
static char buf[CAPACITY];
static char* start = buf;

char* alloc_mem(const int n) {
  // The last element of the buffer is buf[CAPACITY - 1], or, *(buf + CAPACITY - 1)
  // So the exclusive upper bound, the memory element just after the last element, is -
  // &buf[CAPACITY], i.e., (buf + CAPACITY)
  // Similarly the exclusive upper bound of the requested memory is start[n] or (start + n)
  // If the requested upper bound is outside the available upper bound, don't allocate anything.
  if (start + n > buf + CAPACITY) {
    return NULL;
  }

  // The new start position will be one after the last element of the requested memory,
  // i.e., what was previously the exclusive upper bound, now becomes the new start.
  start += n;
  return (start - n);
}

void free_mem(char* p) {
  if (p < buf || p >= buf + CAPACITY) {
    return;  // Do nothing because p is out of bounds
  }

  // Just move the alloc start to the free'ed start.
  start = p;
}



