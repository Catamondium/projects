#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void cat(FILE *f)
{
    for (int ch = fgetc(f); ch != EOF; ch = fgetc(f))
    {
        putchar(ch);
    }
}

int main(int argc, char **argv)
{
    if (argc == 1) // read stdin
    {
        cat(stdin);
    }
    else
    { // read spec in argv
        for (int i = 1; i < argc; ++i)
        {
            int readfile = strcmp(argv[i], "-") != 0;
            FILE *file = (readfile) ? fopen(argv[i], "r") : stdin;
            cat(file);

            if (readfile)
            {
                fclose(file);
            }
        }
    }
}