#include <stdio.h>

main (){
  int INPUT;
  scanf("%d", &INPUT); //input bodge

  int fib(int i) {
    int result;
    for (int n = 0; n == i; n++)
      {
	int first = 0;
	int second = 1;
	result = first + second; //summate next term
	first = second; //prepare next set
	second = result; //prepare next set
      }
    return result;
  }
  
  printf("%d", fib(INPUT)); //output
  return fib(INPUT);
}
