#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define PORT "80" // HTTP check by default

int main(int argc, char **argv) {
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    int sfd, s, j;
    size_t len;
    ssize_t nread;

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;

    hints.ai_socktype = SOCK_STREAM; //SOCK_DGRAM;
    hints.ai_flags = 0;
    hints.ai_protocol = 0;

    if (argc < 2) {
        perror("Host/domain name required");
        exit(EXIT_FAILURE);
    }

    s = getaddrinfo(argv[1], PORT, &hints, &result);
    if(s != 0) {
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
        exit(EXIT_FAILURE);
    }

    for(rp = result; rp != NULL; rp = rp->ai_next) {
        sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sfd == -1)
            continue;
        if (connect(sfd, rp->ai_addr, rp->ai_addrlen) != -1) {
            printf("SUCCESSFUL CONNECT to \"%s\"\n", argv[1]);
            break;
        }
        close(sfd);
    }
}
