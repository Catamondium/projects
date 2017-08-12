#include <stdio.h>

int main () {
  int n = +20; //variable to n=20
  int current; //declare trace
  for (current, n; current <= n; current++); //link trace, loop condition, iterate trace
  {
    int current = 1; //initalise tracer
    printf("%dth:\t%d\n", n, current);}
  return 0;}
