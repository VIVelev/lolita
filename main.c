#include <stdarg.h>
#include <stdio.h>

void print_numbers(int count, ...) {
  va_list args;
  va_start(args, count);

  for (int i = 0; i < count; i++) {
    int num = va_arg(args, int);
    printf("%d ", num);
  }

  va_end(args);
  printf("\n");
}

int main() {
  print_numbers(3, 10, 20, 30);
  return 0;
}
