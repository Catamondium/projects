#include <stdio.h>

main (){
  int i; //global input
  scanf("%d", &i);
  int factorial (int n) { //local input

    if (n == 0) //test empty product, reduction resolve
      {
	return 1;//empty product
      }
    else { //continue n!
      n = n * factorial(n-1); //multiply input by preceding number, recursively
    }
    return n;
  }
  
  printf("%d\n", factorial(i)); //output
  return factorial(i);
}
