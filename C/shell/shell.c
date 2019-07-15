#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define READLINE
#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif
#include <readline/tilde.h>

#include "csh_exec.h"

#define BUFSIZE_CSH 1024
#define TOK_DELIM " \t\r\n\a"

/* TODO
 * proper tokenization: acknowledge " " strings & escapes
 */

#ifndef READLINE
// Readline does exist, but is example of dynamic array
char *readline(const char *prompt)
{
    if (prompt)
        printf("%s", prompt);

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
#endif

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
    char *line = NULL, *expanded = NULL;
    char **args = NULL;
    int status = false;
    do
    {
        line = readline("CSH> ");
        if (!line)
            break;
        expanded = tilde_expand(line);
        args = tokenize(expanded);
        status = csh_exec(args);

#ifdef READLINE
        add_history(line);
#endif
        free(line);
        free(expanded);
        free(args);
    } while (status);
}

#ifdef READLINE
void init_readline()
{
    rl_bind_key('\t', rl_complete);
}
#endif

int main()
{
#ifdef READLINE
    init_readline();
#endif
    csh_loop();
}