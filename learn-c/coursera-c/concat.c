#include <stdio.h>
#include <stdlib.h>
#include <string.h>

size_t length(const char* str) {
  size_t len = 0;
  while (str[len]) len++;
  return len;
}

void concat(char** p_src, const char* dest) {
  const char* src = *p_src;
  char* full = (char*)malloc(length(src) + length(dest) + 1);
  char* start = full;
  memcpy(full, src, length(src));
  full += length(src);
  memcpy(full, dest, length(dest));
  full += length(dest);
  *full = '\0';
  free(*p_src);
  *p_src = start;
}



int main(int argc, char** argv) {
  char* fname = strdup("Avilay");
  char* lname = strdup(" Parekh");
  concat(&fname, lname);
  printf("%s\n", fname);
}