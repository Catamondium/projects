#include <stdio.h>

int main (){
  int x = 3;
  int y = factorial(x);
  printf("%d\n", y); //output
  return factorial(i);
}

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
