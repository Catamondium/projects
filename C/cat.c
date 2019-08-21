#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define handle_error(msg)   \
    do                      \
    {                       \
        perror(msg);        \
        putchar('\n');      \
        exit(EXIT_FAILURE); \
    } while (0)

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
            int readstd = strcmp(argv[i], "-") == 0;
            FILE *file = (readstd) ? stdin : fopen(argv[i], "r");
            cat(file);

            if (!readstd)
            {
                fclose(file);
            }
        }
    }
}