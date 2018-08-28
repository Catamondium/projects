#include <math.h> // floor
#include <stdio.h>

typedef struct Time {
    unsigned int hrs;
    unsigned int mins;
} Time;

Time doElapse(const Time s, const signed int t) {
    Time ret;

    int offset = s.hrs * 60 + s.mins;
    int tot = offset + t;
    ret.hrs = floor(tot / 60);
    ret.mins = tot % 60;

    return ret;
}

void sTime(char* ret, const Time t) {
	sprintf(ret, "%02d:%02d", t.hrs, t.mins);
	return;
}

int main(int argc, char *argv[]) {
    Time start, end;
    char strStart[20], strEnd[20];
    signed int elapse;

    if(argc < 3) {
        printf("Error:\thh:mm mins expected.\n");
        return 1;
    }

    sscanf(argv[1], "%u:%u", &start.hrs, &start.mins);
    sscanf(argv[2], "%d", &elapse);
    end = doElapse(start, elapse);

    sTime(strStart, start);
    sTime(strEnd, end);

    printf("Start:\t%s\t%+d\nEnd:\t%s\n", strStart, elapse, strEnd);
    return 0;
}
