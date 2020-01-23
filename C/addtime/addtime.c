#include <getopt.h>
#include <math.h>               // floor
#include <stdio.h>
#include <stdlib.h>             // exit, atoi
#include <string.h>             // strstr
#include <unistd.h>             // getopt

typedef struct Time {
    unsigned int hrs;
    unsigned int mins;
} Time;

void sTime(char *ret, const Time t)
{
    sprintf(ret, "%02d:%02d", t.hrs, t.mins);
}

Time pTime(const char *str)
{
    Time ret;
    sscanf(str, "%u:%u", &ret.hrs, &ret.mins);
    return ret;
}

int getMins(Time t)
{
    return (t.hrs * 60) + t.mins;
}

Time doElapse(const Time s, const signed int t)
{
    int tot = getMins(s) + t;
    return (Time) {
    floor(tot / 60), tot % 60};
}

void usage(const char *prog)
{
    printf("Usage: %s [-hql] <HH:MM> <mins | HH:MM>\n "
           "Note: if mins_elapse is negative, precede it with '--'\n"
           "Options:\n\t-q quietly output end time\n"
           "\t-h print this message and exit\n"
           "\t-l print written language\n", prog);
    exit(1);
}

int main(int argc, char **argv)
{
    Time start, end;
    char strStart[20], strEnd[20];
    signed int elapse;
    int quiet = 0;

    int c;
    while ((c = getopt(argc, argv, "qhl")) != -1) {
        switch (c) {
        case 'q':
            quiet = 1;
            break;
        case 'l':
            printf("C: %ld\n", __STDC_VERSION__);
            return 0;
        default:
            usage(argv[0]);
            break;
        }
    }

    if (argc < 3)
        usage(argv[0]);

    start = pTime(argv[optind++]);
    elapse =
        strstr(argv[optind],
               ":") !=
        NULL ? getMins(pTime(argv[optind])) : atoi(argv[optind]);

    end = doElapse(start, elapse);
    if (!quiet) {
        sTime(strStart, start);
        sTime(strEnd, end);
        printf("Start:\t%s\t%+d\nEnd:\t%s\n", strStart, elapse, strEnd);
    } else
        printf("%02d:%02d\n", end.hrs, end.mins);
}
