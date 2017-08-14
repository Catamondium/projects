#include <stdio.h>

main () {
  printf("1.\tRandom number\nSelect:\t");
  int n;
  int rand(void) //initialise random, CHECK
  scanf("%d", n); //take input
  switch (n) {
  case 1:
    printf("%d", rand() );
    break;
  default:
    printf("Bad input!");
  };
  return 0
    };
