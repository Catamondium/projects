#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>

#define END "|- "
#define INDENT " "

/*
 * Recursively lists files
 * basic tree
 */

void tree(char *dirstr, char *indent)
{
    DIR *dir = NULL;
    dir = opendir(dirstr);
    if (dir != NULL) {
	struct dirent *entry = NULL;
	while ((entry = readdir(dir)) != NULL) {
	    if (strcmp(".", entry->d_name) == 0)
		continue;
	    if (strcmp("..", entry->d_name) == 0)
		continue;

	    printf("%s" END "%s\n", indent, entry->d_name);
	    if (entry->d_type == DT_DIR) {
		char *path = NULL;
		char *nident = NULL;
		asprintf(&path, "%s/%s", dirstr, entry->d_name);
		asprintf(&nident, "%s" INDENT, indent);
		if (path == NULL || nident == NULL)
		    continue;
		tree(path, nident);
		free(path);
		free(nident);
	    }
	}

	closedir(dir);
    }
}

int main(int argc, char **argv)
{
    for (int i = 1; i < argc; ++i) {
	struct stat statbuf;

	if (lstat(argv[i], &statbuf) == -1) {
	    perror("stat");
	    continue;
	}

	if ((statbuf.st_mode & S_IFMT) == S_IFDIR) {
	    puts(argv[i]);
	    tree(argv[i], INDENT);
	} else
	    puts(argv[i]);
    }
}
