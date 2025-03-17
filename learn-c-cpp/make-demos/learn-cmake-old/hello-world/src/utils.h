#pragma once

extern const int gDefaultCalories;
extern const char* const gDefaultFlavor;

void bake(int calories, const char* flavor);
void serve(int calories, const char* flavor, int numServings);
int getCountInvocations();