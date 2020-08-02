#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <ctype.h>

#define handle_error(call) onerr(call, __LINE__)
static void onerr(int call, int line) {
    if (call == -1) {
        perror("Error");
        printf("line: %d\n", line);
        exit(EXIT_FAILURE);
    }
}

// quote acknowledging, escaped tokenization
char **tokenize(char *line)
{
    size_t len = strlen(line);
    bool inquote = false;
    char **ptrs = malloc(sizeof(char *) * len);
    if (!ptrs)
        handle_error("Allocation error");
    size_t position = 0;
    bool inreal = false;

    for (int i = 0; i < len; ++i)
    {
        if (line[i] == '\\') // skip escaped
        {
            if (!inreal)
            {
                inreal = true;
                ptrs[position++] = line + i;
            }

            ++i;
        }
        else if (line[i] == '\"') // entered real quote
        {
            inreal = false;
            inquote = !inquote;
            line[i] = 0;
        }
        else if (!inquote && isspace(line[i])) // entered real space
        {
            inreal = false;
            line[i] = 0;
        }
        else if (!inreal)
        {
            inreal = true;
            ptrs[position++] = line + i;
        }
    }

    ptrs[position] = NULL;
    return ptrs;
}