#pragma once
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define handle_error(call) onerr(call, __LINE__)
static void onerr(int call, int line) {
    if (call == -1) {
    perror("Error");
    printf("line: %d\n", line);
    exit(EXIT_FAILURE);
    }
}

typedef int (*builtin_fun)(char **);
int csh_cd(char **args);
int csh_help(char **args);
int csh_exit(char **args);
int csh_enum(char **args);

typedef struct
{
    char *label;
    builtin_fun fun;
} Builtin;

#define NBUILTINS sizeof(builtins) / sizeof(Builtin)
Builtin builtins[] = {
    {"cd", csh_cd},
    {"exit", csh_exit},
    {"help", csh_help},
    {"enum", csh_enum},
    {"enumerate", csh_enum},
};

int csh_cd(char **args)
{
    if (args[1] == NULL)
    {
        fprintf(stderr, "Missing argument to \"cd\"\n");
        return 1;
    }

    if (chdir(args[1]) != 0)
        handle_error("csh");

    return 1;
}

int csh_exit(char **args)
{
    return 0;
}

int csh_enum(char **args)
{
    for (char **arg = (args + 1); *arg != NULL; ++arg)
    {
        printf("\'%s\'\n", *arg);
    }
    return 1;
}

int csh_help(char **args)
{
    printf(
        "C shell builtins:\n"
        "\tcd: Change working directory\n"
        "\t\t->> relative paths not supported\n"
        "\texit: leave the shell\n"
        "\thelp: display this message\n"
        "\tenumerate: list args quoted\n");
    return 1;
}

int csh_syscall(char **args)
{
    pid_t pid, wpid;
    int status;

    pid = fork();
    if (pid == 0)
    {
        // child
        if (execvp(args[0], args) == -1)
            handle_error("csh");
    }
    else if (pid > 0)
    {
        // parent
        do
        {
            wpid = waitpid(pid, &status, WUNTRACED);
        } while (!WIFEXITED(status) && !WIFSIGNALED(status));
    }
    else
        handle_error("csh");

    return 1;
}

char *getHome()
{
    char *homedir;
    if ((homedir = getenv("HOME")) == NULL)
        homedir = getpwuid(getuid())->pw_dir;

    if (!homedir)
        handle_error("csh");

    return homedir;
}

int csh_exec(char **args)
{
    if (args[0] == NULL)
        return 1;

    for (int i = 0; i < NBUILTINS; ++i)
    {
        if (strcmp(args[0], builtins[i].label) == 0)
        {
            return (builtins[i].fun)(args);
        }
    }
    return csh_syscall(args);
}