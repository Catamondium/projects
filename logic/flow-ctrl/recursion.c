#include <stdio.h>

int n;
int factorial (n){
  
  if ((n == 1) or (n == 0)) //test empty product, reduction resolve
  {
    printf("1");
    return 1;//empty product
    }
  
  else { //continue n!
    n * factorial(n-1); //multiply input by preceding number, recursively
    printf("%d", int factorial(n));
      }
  return n;
}
