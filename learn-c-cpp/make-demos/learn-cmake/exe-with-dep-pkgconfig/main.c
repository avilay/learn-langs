#include <glib.h>
#include <stdio.h>

int main() {
  GArray* arr = g_array_sized_new(FALSE, TRUE, sizeof(int), 16);
  int x = 1;
  g_array_append_val(arr, x);

  int xs[3] = {10, 20, 30};
  g_array_append_vals(arr, &xs, 3);

  for (int i = 0; i < arr->len; i++) {
    printf("arr[%d] = %d\n", i, g_array_index(arr, int, i));
  }
}