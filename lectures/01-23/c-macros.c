#include <stdio.h>
#include <assert.h>

#define SQUARE(x) x * x

int main() {
  int result = SQUARE(2 + 3);
  printf("result: %d\n", result);
}