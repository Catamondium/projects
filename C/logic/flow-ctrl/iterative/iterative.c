#include <stdio.h>

int main(void){
  int num = 7;
  int y = fibbonacci(num);
  printf("%d\n", y);
}

int fibonacci(int i){
  int first = 0;
  int second = 1;
  int result;
  
  switch(i) {
	  case 0:
	  result = 0;
	  break;
	  case 1:
	  result = 1;
	  break;
	  default:
	      for (int n = 1; n < i; n++) {
      result = first + second;
      first = second;
      second = result;
    }
    break;
  }
  return result;
}
