#include "utils.h"
#include <stdio.h>
#include <string.h>

const int gDefaultCalories = 200;
const char* const gDefaultFlavor = "Chocolate Chip";
static int countInvocations = 0;

void bake(int calories, const char* flavor) {
  static _Thread_local int countBakeInvocations = 0;
  countBakeInvocations += 1;
  countInvocations += 1;
  printf("DEBUG: bake has been invoked %d times.\n", countBakeInvocations);
  if (calories == 0) {
    calories = gDefaultCalories;
  }
  if (strcmp(flavor, "") == 0) {
    flavor = gDefaultFlavor;
  }
  printf("Baking %s cookie with %d calories.\n", flavor, calories);
}

void serve(int calories, const char* flavor, int numServings) {
  static _Thread_local int countServeInvocations = 0;
  countServeInvocations += 1;
  countInvocations += 1;
  printf("DEBUG: serve has been invoked %d times.\n", countServeInvocations);
  if (numServings <= 0) {
    return;
  }
  if (calories == 0) {
    calories = gDefaultCalories;
  }
  int res = strcmp(flavor, "");
  if (res == 0) {
    flavor = gDefaultFlavor;
  }
  printf("Serving %d %s cookies with %d calories each.\n", numServings, flavor,
         calories);
}

int getCountInvocations() { return countInvocations; }