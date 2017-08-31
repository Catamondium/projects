#include <stdio.h>

main (){
  int INPUT;
  scanf("%d", &INPUT); //input bodge

  int fib(int i) {
    int result;
    if (i == 0) {return 0;}
    if (i == 1) {return 1;}
    else
      {
	for (int n = 0; n <= i; n++)
	  {
	    int first = 0;
	    int second = 1;
	    result = first + second; //summate next term
	    first = second; //prepare next set
	    second = result; //prepare next set
	  }
	return result;
      }
  }
  
  printf("%d\n", fib(INPUT)); //output
  return fib(INPUT);
}
