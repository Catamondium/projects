#define _GNU_SOURCE // asprintf
#include <math.h> // floor
#include <stdlib.h> // atoi
#include <stdio.h>

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
        printf("Error:\thh:mm mins expected.\n");
        return 1;
    }

    sscanf(argv[1], "%d:%d", &start.hrs, &start.mins);
    elapse = atoi(argv[2]);

    end = calcEtime(start, elapse);
    // Build output string
    char *Ostr_S, *Ostr_E;
    asprintf(&Ostr_S, "Start:\t%02i:%02i", start.hrs, start.mins);
    asprintf(&Ostr_E, "End:\t%02i:%02i\n", end.hrs, end.mins);
    printf("%s\t%+d\n%s", Ostr_S, elapse, Ostr_E);
    free(Ostr_S);
    free(Ostr_E);
    return 0;
}
