#include <locale.h>
#include <stdlib.h>
#include <stdio.h>
typedef long long int big_number;

big_number fibonacci(int n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
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
	return 1;
    }

    int n = atoi(argv[1]);
    for(int i = 0; i < n; ++i) {
	    printf("%lld\n", fibonacci(i));
    }
    return 0;
}
