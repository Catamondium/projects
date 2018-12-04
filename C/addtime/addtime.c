#include <stdio.h>
#include <math.h> // floor
#include <unistd.h> // getopt
#include <stdlib.h> // exit, atoi
#include <string.h> // strstr

typedef struct Time {
    unsigned int hrs;
    unsigned int mins;
} Time;

void sTime(char *ret, const Time t) {
	sprintf(ret, "%02d:%02d", t.hrs, t.mins);
}

Time pTime(const char *str) {
	Time ret;
	sscanf(str, "%u:%u", &ret.hrs, &ret.mins);
	return ret;
}

int getMins(Time t) {
	return (t.hrs * 60) + t.mins;
}

Time doElapse(const Time s, const signed int t) {
    Time ret;

    int tot = getMins(s) + t;
    ret.hrs = floor(tot / 60);
    ret.mins = tot % 60;

    return ret;
}

void usage(const char *prog) {
	printf("Usage: %s [-qh] hh:mm mins_elapse\n", prog);
	printf("Note: if mins_elapse is negative, precede it with '--'\n");
	printf("Options:\n\t-q quietly output end time\n");
	printf("\t-h print this message and exit\n");
	exit(1);
}

int main(int argc, char **argv) {
    Time start, end;
    char strStart[20], strEnd[20];
    signed int elapse;
    int quiet = 0; // default to human readable

    int c;
    while((c = getopt(argc, argv, "qh")) != -1) {
	    switch(c) { // avoids gcc moaning about type issue
		    case 'q':
			    quiet = 1;
			    break;
		    default:
			    usage(argv[0]);
			    break;
	    }
    }

    if(argc < 3) {
	    usage(argv[0]);
    }

    start = pTime(argv[optind++]);

    elapse = strstr(argv[optind], ":") != NULL ?
	    getMins(pTime(argv[optind])) : atoi(argv[optind]);

    end = doElapse(start, elapse);

    if(!quiet) {
	    sTime(strStart, start);
	    sTime(strEnd, end);
	    printf("Start:\t%s\t%+d\nEnd:\t%s\n",
			    strStart, elapse, strEnd);
    } else
	    printf("%02d:%02d\n", end.hrs, end.mins);

    return 0;
}
