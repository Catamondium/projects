#include <stdio.h>

main () {
  printf("Number:\t");
  float n;
  scanf("%f", n);
  switch (n) {
    
  case n < 0:
    printf("Number is negative");
    break;
    
  case n > 0:
    printf("Number is positive");
    break;
    
  default:
    printf("Number is 0");
    break;
  };
  return 0;
    };
