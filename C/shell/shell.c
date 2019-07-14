#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <sys/wait.h>

#define BUFSIZE_CSH 1024
#define TOK_DELIM " \t\r\n\a"
#define handle_error(msg)   \
    do                      \
    {                       \
        perror(msg);        \
        exit(EXIT_FAILURE); \
    } while (false)

/* TODO
 * proper tokenization: acknowledge " " strings & escapes
 */

#define NBUILTINS 2
typedef int (*builtin_fun)(char *);
int csh_cd(char **args);
//TODO int csh_help(char **args);
int csh_exit(char **args);

char *builtins_str[] = {
    "cd",
    "exit",
    //"help"
};

int (*builtins[])(char **) = {
    &csh_cd,
    &csh_exit,
    //&csh_help
};

int csh_cd(char **args)
{
    char *resolved = NULL;

    if (args[1] == NULL)
    {
        fprintf(stderr, "Missing argument to \"cd\"");
        return 1;
    }

    resolved = realpath(args[1], resolved);
    if (!resolved)
        handle_error("csh");

    if (chdir(resolved) != 0)
        handle_error("csh");
    return 1;
}

int csh_exit(char **args)
{
    return 0;
}

// Readline does exist, but is example of dynamic array
char *getLine()
{
    size_t bufsize = BUFSIZE_CSH, position = 0;
    char *buffer = malloc(bufsize * sizeof(char));
    int ch;

    if (!buffer)
        handle_error("Bad allocation");

    while (true)
    {
        ch = getchar();

        if (ch == '\n' || ch == EOF)
        {
            buffer[position] = '\0';
            return buffer;
        }

        buffer[position++] = ch;
        if (position >= bufsize)
        {
            bufsize += BUFSIZE_CSH;
            buffer = realloc(buffer, bufsize * sizeof(char));
            if (!buffer)
                handle_error("Bad allocation");
        }
    }
    return buffer;
}

char **tokenize(char *line)
{
    size_t bufsize = BUFSIZE_CSH, position = 0;
    char **tokens = malloc(bufsize * sizeof(char *));
    char *tok;

    if (!tokens)
        handle_error("Bad allocation");

    tok = strtok(line, TOK_DELIM);
    while (tok != NULL)
    {
        tokens[position++] = tok;

        if (position >= bufsize)
        {
            bufsize += BUFSIZE_CSH;
            tokens = realloc(tokens, bufsize * sizeof(char *));
            if (!tokens)
                handle_error("Bad allocation");
        }

        tok = strtok(NULL, TOK_DELIM);
    }
    tokens[position] = NULL;
    return tokens;
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

void csh_loop()
{
    char *line = NULL;
    char **args = NULL;
    int status = false;
    do
    {
        printf("CSH> ");
        line = getLine();
        args = tokenize(line);
        status = csh_exec(args);

        free(line);
        free(args);
    } while (status);
}

int main()
{
    csh_loop();
}