#include <stdio.h>

main(){
  int INPUT;
  scanf("%d", &INPUT);
  printf("%d\n", fibonacci(INPUT));
}
fibonacci(int i){
  int first = 0;
  int second = 1;
  int result;

  if (i == 0)
    {return 0;}
  if (i == 1)
    {return 1;}

  else {
    for (int n = 1; n < i; n++) {
      result = first + second;
      first = second;
      second = result;
    }
    return result;
  }
}
