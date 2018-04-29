#include <stdio.h>
#include <stdlib.h>

unsigned long long int fibonacci(int n) {
    if (n == 0) {
        return 0;
    }
    if (n == 1) {
        return 1;
    } else {
        unsigned long long int first = 0;
        unsigned long long int second = 1;
        unsigned long long int result;
        for (int i = 1; i < n; i++) {
            result = first + second;
            first = second;
            second = result;
        }
        return result;
    }
}

int main(int argc, char *argv[]) {
    if (argc == 1 || atoi(argv[1]) < 0) {
        printf("Error: Positive argument required.\n");
    } else {
        int n = atoi(argv[1]);
        for(int i = 0; i < n; ++i) {
            printf("%lld\t", fibonacci(i));
            if((i + 1) % 7 == 0 && i != 0) {
                printf("\n");
            }
        }
        printf("\n");
    }
}
