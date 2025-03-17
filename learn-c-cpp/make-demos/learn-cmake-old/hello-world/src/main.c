#include "utils.h"
#include <stdio.h>

int main() {
  const int calories = 180;
  const char* flavor = "Oatmeal Raisin";
  bake(calories, flavor);
  serve(0, "", 3);
  serve(220, "Snicker Doodle", 5);
  printf("Default calories are %d\n", gDefaultCalories);
  printf("Number of invocations: %d\n", getCountInvocations());
}
