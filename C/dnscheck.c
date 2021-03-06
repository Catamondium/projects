#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <ctype.h>
#include <string.h>

#define HTTP "80"		// HTTP check by default

typedef struct Context {
    char *port;
    bool palloc;		// port allocation indicator
    char *host;
    int type;
} Context;

void help(char *prog, int status)
{
    printf("Usage: %s HOST|DOMAIN[:PORT] [-p PORT] [-DhH]\n"
	   "PORT defaults to 80 (HTTP check)\n"
	   "OPTIONS:\n"
	   "\t-p : port select (overrides HOST:PORT)\n"
	   "\t-D : test over dcp transport\n"
	   "\t -h, -H : display this message\n", prog);
    exit(status);
}

Context argparse(int argc, char **argv)
{
    int opt;
    Context ctx = {
	.port = HTTP,
	.palloc = false,
	.host = NULL,
	.type = SOCK_STREAM,
    };

    while ((opt = getopt(argc, argv, "HhDp:")) != -1) {
	switch (opt) {
	case 'D':
	    ctx.type = SOCK_DGRAM;
	    break;
	case 'p':
	    ctx.port = strdup(optarg);
	    ctx.palloc = true;
	    break;
	case 'h':
	case 'H':
	    help(argv[0], EXIT_SUCCESS);
	    break;
	}
    }

    if (argc - optind < 1) {
	fprintf(stderr, "Error: Host required\n");
	help(argv[0], EXIT_FAILURE);
    }

    char *dptr = strchr(argv[optind], ':');
    if (dptr != NULL) {
	// Extract port from host:port notation
	// don't override -p
	if (!ctx.palloc && isdigit(dptr[1]))
	    ctx.port = dptr + 1;
	dptr[0] = '\0';
    }

    ctx.host = argv[optind];

    return ctx;
}

int main(int argc, char **argv)
{
    Context ctx;
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    int sfd, s;

    ctx = argparse(argc, argv);

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;

    hints.ai_socktype = ctx.type;
    hints.ai_flags = 0;
    hints.ai_protocol = 0;

    s = getaddrinfo(ctx.host, ctx.port, &hints, &result);
    if (s != 0) {
	fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
	exit(EXIT_FAILURE);
    }

    int success = 0;
    int total = 0;

    for (rp = result; rp != NULL; rp = rp->ai_next) {
	sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
	if (sfd == -1) {
	    perror("socket error");
	} else if (connect(sfd, rp->ai_addr, rp->ai_addrlen) != -1) {
	    printf("SUCCESS %s:%s\n", ctx.host, ctx.port);
	    ++success;
	    close(sfd);
	} else {
	    perror("connect error");
	}
	++total;
    }

    if (result == NULL) {
	puts("No results");
    } else {
	printf("success %d / %d => %02d%%\n", success, total,
	       (100 * success) / total);
	freeaddrinfo(result);
	if (ctx.palloc)
	    free(ctx.port);

    }
}
