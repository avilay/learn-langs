// char *ReprArray(int *arr, int sz) {
//   int capacity = 128;
//   char *repr = (char *)calloc(capacity, sizeof(char));
//   int tot_bytes = 0;
//   for (int i = 0; i < sz; i++) {
//     char *val;
//     tot_bytes += asprintf(&val, "%d ", arr[i]);
//     if (tot_bytes >= capacity) {
//       capacity += 1024;
//       repr = (char *)realloc(repr, capacity);
//     }
//     strlcat(repr, val, capacity);
//   }
//   return repr;
// }

// ListNode *CreateList(int *vals, int sz) {
//   ListNode *head = (ListNode *)malloc(sizeof(ListNode));
//   ListNode *prev = head;
//   prev->val = vals[0];
//   for (int i = 1; i < sz; i++) {
//     ListNode *curr = (ListNode *)malloc(sizeof(ListNode));
//     curr->val = vals[i];
//     prev->next = curr;
//     prev = curr;
//   }
//   return head;
// }

// void PrintList(ListNode *head) {
//   ListNode *curr = head;
//   while (curr != NULL) {
//     printf("%d -> ", curr->val);
//     curr = curr->next;
//   }
//   printf("NULL\n");
// }
