#include <stdio.h>

int main() {
  int n;
  printf("enter integer: "); //input
  scanf("%d/n", &n);

  if (n % 2 == 0) { //test integer response
    printf("Even\n"); //if true
  }
  else {
    printf("Odd\n"); //if false
  }
  return (0);
}
