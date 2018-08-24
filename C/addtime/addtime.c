#include <math.h> // floor
#include <stdlib.h> // atoi
#include <stdio.h>

typedef struct Time {
    unsigned int hrs;
    unsigned int mins;
} Time;

Time doElapse(const Time s, const signed int t) {
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
        printf("Error:\thh:mm mins expected.\n");
        return 1;
    }

    sscanf(argv[1], "%u:%u", &start.hrs, &start.mins);
    elapse = atoi(argv[2]);
    end = doElapse(start, elapse);

    printf("Start:\t%02u:%02u\t%+d\nEnd:\t%02u:%02u\n",
		    start.hrs, start.mins, elapse,
		    end.hrs, end.mins);
    return 0;
}
