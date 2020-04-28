#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define PORT "80"		// HTTP check by default

int main(int argc, char **argv)
{
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    int sfd, s;
    //size_t len;
    //ssize_t nread;

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;

    hints.ai_socktype = SOCK_STREAM;	//SOCK_DGRAM;
    hints.ai_flags = 0;
    hints.ai_protocol = 0;

    if (argc < 2) {
	fprintf(stderr, "Host/domain name required");
	exit(EXIT_FAILURE);
    }

    s = getaddrinfo(argv[1], PORT, &hints, &result);
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
	    printf("SUCCESSFUL CONNECT to \"%s\"\n", argv[1]);
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

    }
}
