#include <stdio.h>

main () {
  printf("1.\tRandom number\nSelect:\t");
  int n;
  int rand(void)
  scanf("%d", n);
  switch (n) {
  case 1:
    printf("%d", rand() );
    break;
  default:
    printf("Bad input!");
  };
  return 0
    };
