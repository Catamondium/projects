#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <unistd.h>

/*
 * Recursively lists files
 * basic ls with -r, printing unrooted names
 */

void ls(char *dirstr)
{
    DIR *dir = NULL;
    dir = opendir(dirstr);
    if (dir != NULL)
    {
        struct dirent *entry = NULL;
        while((entry = readdir(dir)) != NULL)
        {
            if(strcmp(".", entry->d_name) == 0)
                continue;
            if(strcmp("..", entry->d_name) == 0)
                continue;

            if(entry->d_type == DT_DIR)
            {
                char *path = NULL;
                asprintf(&path, "%s/%s", dirstr, entry->d_name);
                if(path == NULL)
                    continue;
                ls(path);
                free(path);
            }
            else
                puts(entry->d_name);
        }

        closedir(dir);
    }
}

int main(int argc, char **argv)
{
    for(int i = 1; i < argc; ++i)
    {
        struct stat statbuf;

        if(lstat(argv[i], &statbuf) == -1)
        {
            perror("stat");
            continue;
        }

        if((statbuf.st_mode & S_IFMT) == S_IFDIR)
            ls(argv[i]);
        else
            puts(argv[i]);
    }
}
