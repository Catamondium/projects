#include <math.h>
#include <stdio.h>

int main() {
  int n;
  printf("enter integer\n");
  scanf("%d", &n);

  if (n % 2 == 0) {
    printf("Number is even\n");
  }
  else {
    printf("number is odd\n");
  }
  return (0);
}
