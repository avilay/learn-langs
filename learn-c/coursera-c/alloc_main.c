//
// Created by avilay on 10/14/25.
//
#include <stdio.h>

#include "alloc.h"

int main() {
  char* first = alloc_mem(512);
  if (!first) {
    printf("Failed to allocate first chunk of memory\n");
  }
  char* second = alloc_mem(512);
  if (!second) {
    printf("Failed to allocate second chunk of memory\n");
  }
  char* third = alloc_mem(512);
  if (!third) {
    printf("Failed to allocate third chunk of memory\n");
  }
}