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

    for (int i = 0; i < len; ++i)
    {
        if (line[i] == '\\') // skip escaped
        {
            ++i;
        }
        else if (line[i] == '\"') // entered real quote
        {
            line[i] = 0;
            inquote = !inquote;
        }
        else if (!inquote && isspace(line[i])) // entered real space
        {
            line[i] = 0;
        }
    }
}

// quote acknowledging, escaped tokenization
char **tokenize(char *line)
{
    size_t len = strlen(line);
    bool inquote = false;
    char **ptrs = malloc(sizeof(char *) * len);
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
            line[i] = 0;
            inquote = !inquote;
        }
        else if (!inquote && isspace(line[i])) // entered real space
        {
            inreal = false;
            line[i] = 0;
        }
        else
        {
            if (!inreal)
            {
                inreal = true;
                ptrs[position++] = line + i;
            }
        }
    }

    ptrs[position] = NULL;
    return ptrs;
}