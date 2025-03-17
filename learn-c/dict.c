#include <search.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Cookie {
    int calories;
    char* label;
} Cookie;

ENTRY* CreateEntry(char* flavor, int calories, char* label)
{
    Cookie* cookie = (Cookie*)malloc(sizeof(Cookie));
    cookie->calories = calories;
    cookie->label = label;
    ENTRY* entry = (ENTRY*)malloc(sizeof(ENTRY));
    entry->key = (char*)malloc(strlen(flavor));
    strcpy(entry->key, flavor);
    entry->data = cookie;
    return entry;
}

int main(int argc, char* argv[])
{
    hcreate(100);
    ENTRY* e1 = CreateEntry("chocolate chip", 200, "Delicious");
    hsearch(*e1, ENTER);

    ENTRY* e2 = CreateEntry("snicker doodle", 180, "Very Nice");
    hsearch(*e2, ENTER);

    ENTRY* e3 = CreateEntry("oatmeal raisin", 150, "Wholesome");
    hsearch(*e3, ENTER);

    ENTRY qry;
    qry.key = "chocolate chip";
    ENTRY* res = hsearch(qry, FIND);
    Cookie* cookie = (Cookie*)res->data;
    printf("Found %s cookie with %d calories and is %s\n", qry.key,
        cookie->calories, cookie->label);

    hdestroy();

    free(e1->data);
    free(e1);

    free(e2->data);
    free(e2);

    free(e3->data);
    free(e3);
}