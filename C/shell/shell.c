#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "csh_exec.h"

#define BUFSIZE_CSH 1024
#define TOK_DELIM " \t\r\n\a"

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