#include <locale.h>
#include <stdlib.h>
#include <stdio.h>
typedef long long int big_number;

big_number fibonacci(int n) {
    if (n == 0) {
        return 0;
    }
    if (n == 1) {
        return 1;
    } else {
        big_number first = 0;
        big_number second = 1;
        big_number result;
        for (int i = 1; i < n; i++) {
            result = first + second;
            first = second;
            second = result;
        }
        return result;
    }
}

int main(int argc, char *argv[]) {
    setlocale(LC_NUMERIC, "");
    if (argc == 1 || atoi(argv[1]) < 0) {
        printf("Error: Positive argument required.\n");
    } else {
        int n = atoi(argv[1]);
        for(int i = 0; i < n; ++i) {
            big_number z = fibonacci(i);

            if (z < 0) {
                printf("Limit at:\t%dth", i-1);
                break;

            } else {
                printf("%lld\t", z);
                if((i + 1) % 7 == 0 && i != 0) {
                    printf("\n");
                }
            }
        }
        printf("\n");
    }
}
