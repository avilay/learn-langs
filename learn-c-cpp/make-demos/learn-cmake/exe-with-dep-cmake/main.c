#include <json-c/json.h>
#include <stdio.h>

int main() {
  json_object* root = json_object_from_file("contacts.json");
  if (!root)
    return 1;

  printf("The json file:\n\n%s\n", json_object_to_json_string(root));
  json_object_put(root);
  return 0;
}