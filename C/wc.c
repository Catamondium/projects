#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

typedef struct Wordcount
{
    int newlines;
    int words;
    int bytes;
    char *name;
} Wordcount;

void sum(Wordcount *acc, Wordcount entry)
{
    acc->bytes += entry.bytes;
    acc->words += entry.words;
    acc->newlines += entry.newlines;
}

Wordcount wc(FILE *f, char *fname)
{
    Wordcount wcount = {0, 0, 0, fname};
    /* We'll define a 'word' as
     * any N chars surrounded by whitespace
     * where N != 0
     */

    int inword = 0;
    for (int ch = fgetc(f); ch != EOF; ch = fgetc(f))
    {
        if (isspace(ch))
        {
            if (ch == '\n')
                ++wcount.newlines;
            inword = 0;
        }
        else if (!inword)
        {
            inword = 1;
            ++wcount.words;
        }
        ++wcount.bytes;
    }

    return wcount;
}

int main(int argc, char **argv)
{
    if (argc == 1) // read stdin
    {
        Wordcount in = wc(stdin, "-");
        printf("%d %d %d %s", in.newlines, in.words, in.bytes, in.name);
    }
    else
    { // read spec in argv
        Wordcount total = {0, 0, 0, "total"};
        for (int i = 1; i < argc; ++i)
        {
            int readfile = strcmp(argv[i], "-") != 0;
            FILE *file = (readfile) ? fopen(argv[i], "r") : stdin;
            Wordcount fcount = wc(file, argv[i]);
            printf("%d %d %d %s\n", fcount.newlines, fcount.words, fcount.bytes, fcount.name);

            sum(&total, fcount);

            if (readfile)
            {
                fclose(file);
            }
        }

        if (argc > 2)
        {
            printf("%d %d %d %s\n", total.newlines, total.words, total.bytes, total.name);
        }
    }
}