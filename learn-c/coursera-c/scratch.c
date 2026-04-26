//
// Created by avilay on 10/14/25.
//
#include <stdio.h>

int add(int x, int y) {
  return x + y;
}

int sub(int x, int y) {
  return x - y;
}

int account(int x, int y, int (*op)(int, int)) {
  return op(x, y);
}

int main() {
  int z = account(1, 2, add);
  printf("%d\n", z);

  int c = account(1, 2, sub);
  printf("%d\n", c);
}