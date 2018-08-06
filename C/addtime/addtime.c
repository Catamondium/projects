#define _GNU_SOURCE
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct Time_s {
    int hrs;
    int mins;
} Time;

Time calcEtime(Time s, int t) {
    Time ret;

    int offset = s.hrs * 60 + s.mins;
    int tot = offset + t;

    ret.hrs = (int) (floor(tot / 60));
    ret.mins = (int) (tot % 60);

    return ret;
}

int fun(int x) {
	return x-1;
}

int main(int argc, char *argv[]) {
    Time start, end;
    signed int elapse;

    if(argc < 3) {
        printf("Error:\tmm:hh mins expected.\n");
        return 1;
    } else {
        elapse = atoi(argv[2]);
        // Split and collect start time
        const char delimiters[] = ":";
        char *input = argv[1];

        char *hrs = strtok(input, delimiters);
        char *mins = strtok(NULL, delimiters);

        start.hrs = atoi(hrs);
        start.mins = atoi(mins);
    }

    end = calcEtime(start, elapse);
    // Build output string
    char *Ostr_S, *Ostr_E;
    asprintf(&Ostr_S, "Start:\t%02i:%02i", start.hrs, start.mins);
    asprintf(&Ostr_E, "End:\t%02i:%02i\n", end.hrs, end.mins);
    printf("%s\t%+d\n%s", Ostr_S, elapse, Ostr_E);
    free(Ostr_S);
    free(Ostr_E);
}
