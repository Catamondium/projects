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
#ifdef DEBUG
    printf("pid: %d\n", pid);
    for (int i = 0; com[i] != NULL; ++i)
        printf("call[%d]: \'%s\'\n", i, com[i]);
#endif

    while (kill(pid, 0) != -1)
        sleep(5);

    if (execvp(com[0], com) == -1)
        perror("exec");
}

int main(int argc, char **argv)
{
    int opt;
    pid_t pid;
    size_t size;

    while ((opt = getopt(argc, argv, "h")) != -1) {
        if (opt == 'h')
            usage(argv[0], EXIT_SUCCESS);
    }

    if (argc <= optind + 1) {
        fprintf(stderr, "expected 2+ arguments after opts\n");
        usage(argv[0], EXIT_FAILURE);
    }

    pid = atoi(argv[optind]);
    ++optind;

    // Build command arguments
    size = argc - optind + 1;
    char *com[size];
    memcpy(com, argv + optind, sizeof(*com) * (size - 1));
    com[size - 1] = NULL;

    assert(com[0] != NULL);
    onexit(pid, com);
}
