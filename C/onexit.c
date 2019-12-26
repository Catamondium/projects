#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <signal.h>
#include <stdio.h>

void usage(char *prog, int status)
{
    printf("Usage: %s [-h] PID COMMAND\n"
	   "PID: process ID to monitor\n"
	   "COMMAND: command on exit\n"
	   "-h: display this usage message\n", prog);
    exit(status);
}

void onexit(pid_t pid, char *com)
{
    while (kill(pid, 0) != -1) {
	sleep(5);
    }

    if (system(com) == -1) {
	perror("system");
    }
}

int main(int argc, char **argv)
{
    int opt;
    while ((opt = getopt(argc, argv, "h")) != -1) {
	if (opt == 'h')
	    usage(argv[0], EXIT_SUCCESS);
    }

    if (optind >= argc - 1) {
	fprintf(stderr, "expected 2 arguments after opts\n");
	usage(argv[0], EXIT_FAILURE);
    }

    onexit(atoi(argv[optind]), argv[optind + 1]);
}
