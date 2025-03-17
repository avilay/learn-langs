#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* oldWay() {
  int value = rand();
  int bufSize = snprintf(NULL, 0, "The random value generated is %d", value);
  char* text = malloc(bufSize);
  snprintf(text, bufSize, "The random value generated is %d", value);
  return text;
}

char* newWay() {
  int value = rand();
  char* text;
  int len = asprintf(&text, "The random value generated is %d", value);
  if (len == -1) {
    printf("The string is too big for memory!\n");
    return NULL;
  }
  return text;
}

void split_demo() {
  char path[] = "/home/avilay/miniconda3/bin:/home/avilay/miniconda3/condabin:"
                "/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:"
                "/usr/bin/vendor_perl:/usr/bin/core_perl";
  printf("path = %s\n\n", path);

  const char* delim = ":";
  char* scratch;

  char* dir = strtok_r(path, delim, &scratch);
  printf("path = %p, path = %s\n", path, path);
  printf("dir = %p, dir = %s\n\n", dir, dir);
  dir = strtok_r(NULL, delim, &scratch);
  char* prev_dir = path;

  while (dir) {
    int offset = strlen(prev_dir) + 1;
    printf("prev_dir + offset = %p, prev_dir + offset = %s\n",
           prev_dir + offset, prev_dir + offset);
    printf("dir = %p, dir = %s\n\n", dir, dir);

    prev_dir = dir;
    dir = strtok_r(NULL, delim, &scratch);
  }
}

int main() {
  char* old = oldWay();
  puts(old);
  free(old);

  char* text = newWay();
  puts(text);
  free(text);
  puts("DONE.");

  split_demo();
}