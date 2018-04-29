#include <stdio.h>
#include <stdlib.h>

long int fibonacci(int n) {
    if (n == 0) {
        return 0;
    }
    if (n == 1) {
        return 1;
    } else {
        long int first = 0;
        long int second = 1;
        long int result;
        for (int i = 1; i < n; i++) {
            result = first + second;
            first = second;
            second = result;
        }
        return result;
    }
}

int main(int argc, char *argv[]) {
    if (argc == 1) {
        printf("Error, integer argument required.\n");
    } else {
        int n = atoi(argv[1]);
        for(int i = 0; i < n; ++i) {
            printf("%ld\t", fibonacci(i));
            if((i + 1) % 7 == 0 && i != 0) {
                printf("\n");
            }
        }
        printf("\n");
    }
}
