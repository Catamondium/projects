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
    char *out_1, *out_2;
    asprintf(&out_1, "Start time:\t%02i:%02i", start.hrs, start.mins);
    asprintf(&out_2, "End time:\t%02i:%02i\n", end.hrs, end.mins);
    printf("%s\t%+d\n%s", out_1, elapse, out_2);
}
