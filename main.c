#include <stdio.h>
#define g(a) \
  int a##_bye = 1;

int main() {
  g(hello);
  printf("%d\n", hello_bye);
}
