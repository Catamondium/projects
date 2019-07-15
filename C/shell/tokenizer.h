#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include <ctype.h>

// PRENULLED input
char **_tok_makeptrs(char *line, size_t len)
{
    char **ptrs = malloc(sizeof(char *) * len);
    size_t position = 0;
    bool inreal = false;

    for (int i = 0; i < len; ++i)
    {
        if (line[i] != 0)
        {                // is real
            if (!inreal) // first new
            {
                ptrs[position++] = line + i;
                inreal = true;
            }
        }
        else
        {
            inreal = false;
        }
    }
    ptrs[position] = NULL;

    return ptrs;
}

void _tok_addnulls(char *line)
{
    bool inquote = false;
    size_t len = strlen(line);

    for (int i = 0; i < len;)
    {
        if (line[i] == '\\') // skip escaped
        {
            i += 2;
        }
        else if (line[i] == '\"') // entered real quote
        {
            line[i] = 0;
            inquote = !inquote;
            ++i;
        }
        else if (!inquote && isspace(line[i])) // entered real space
        {
            line[i] = 0;
            ++i;
        }
        else
            ++i;
    }
}

// quote acknowledging, escaped tokenization
char **tokenize(char *line)
{
    size_t len = strlen(line);
    _tok_addnulls(line);
    char **ptrs = _tok_makeptrs(line, len);
    return ptrs;
}