#pragma once
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define handle_error(msg)   \
    do                      \
    {                       \
        perror(msg);        \
        putchar('\n');      \
        exit(EXIT_FAILURE); \
    } while (false)

/* TODO
 * proper tokenization: acknowledge " " strings & escapes
 */

#define NBUILTINS 3
typedef int (*builtin_fun)(char *);
int csh_cd(char **args);
int csh_help(char **args);
int csh_exit(char **args);
char *canonicalize(char *);

char *builtins_str[] = {
    "cd",
    "exit",
    "help",
};

int (*builtins[])(char **) = {
    &csh_cd,
    &csh_exit,
    &csh_help,
};

int csh_cd(char **args)
{
    if (args[1] == NULL)
    {
        fprintf(stderr, "Missing argument to \"cd\"\n");
        return 1;
    }

    char *resolved = canonicalize(args[1]);
    if (!resolved)
        handle_error("csh");

    if (chdir(resolved) != 0)
        handle_error("csh");

    free(resolved);
    return 1;
}

int csh_exit(char **args)
{
    return 0;
}

int csh_help(char **args)
{
    printf(
        "C shell builtins:\n"
        "\tcd: Change working directory\n"
        "\t\t->> relative paths not supported\n"
        "\texit: leave the shell\n"
        "\thelp: display this message\n");
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

char *canonicalize(char *relpath)
{
    char *buffer;

    if (relpath[0] == '~')
    { // HOME substitution
        char *home = getHome();
        size_t homelen = strlen(home);
        size_t rellen = strlen(relpath);

        buffer = malloc(sizeof(char) * (homelen + rellen));
        if (!buffer)
            handle_error("Allocation error");

        strcpy(buffer, home);
        strcpy(buffer + homelen, &(relpath[1]));
    }
    return buffer;
}

int csh_exec(char **args)
{
    if (args[0] == NULL)
        return 1;

    for (int i = 0; i < NBUILTINS; i++)
    {
        if (strcmp(args[0], builtins_str[i]) == 0)
        {
            return (*builtins[i])(args);
        }
    }
    return csh_syscall(args);
}