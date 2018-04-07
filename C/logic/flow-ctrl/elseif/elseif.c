#include <stdio.h>

int main (void) {
  printf("Integer:\t");
  signed int n;
  scanf("%i", n);
  if(n < 0) {
	  printf("Number is negative.");
  } else if (n > 0) {
	  printf("Number is positive.");
  } else {
	  printf("Number is 0.");
  }
  return 0;
 }
