#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <signal.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

void usage(char *prog, int status)
{
    printf("Usage: %s [-h] PID COMMAND [ARGS]...\n"
	   "\tPID: process ID to monitor\n"
	   "\tCOMMAND: command on exit\n"
	   "\tARGS: command arguments\n"
	   "\t-h: display this usage message\n", prog);
    exit(status);
}

void onexit(pid_t pid, char **com)
{
    while (kill(pid, 0) != -1)
	sleep(5);

    if (execvp(com[0], com) == -1)
	perror("trigger");
}

char **build_com(int argc, int fromn, char **argv)
{
    assert(argc >= fromn + 1);
    size_t size;
    char **com;

    size = sizeof(char *) * (argc - fromn + 1);
    com = malloc(size);
    // argv[fromn:] -> com[:-1], using Python slice notation
    memcpy(com, argv + fromn, size - sizeof(char));
    com[argc - fromn] = NULL;

#ifdef DEBUG
    for (char **i = com; *i != NULL; ++i)
	printf("call: \'%s\'\n", *i);
#endif

    return com;
}

int main(int argc, char **argv)
{
    int opt;
    char **com;

    while ((opt = getopt(argc, argv, "h")) != -1) {
	if (opt == 'h')
	    usage(argv[0], EXIT_SUCCESS);
    }

    if (argc <= optind + 1) {
	fprintf(stderr, "expected 2+ arguments after opts\n");
	usage(argv[0], EXIT_FAILURE);
    }

    com = build_com(argc, optind + 1, argv);
    assert(com[0] != NULL);
    onexit(atoi(argv[optind]), com);
    free(com);
}
