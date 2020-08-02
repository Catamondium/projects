#include <stdio.h>
#include <math.h>		// log10, floor
#include <stdlib.h>		// atoi
#include <ctype.h>		// isdigit

void err()
{
    fprintf(stderr, "integer operand > 0 needed.\n");
    exit(1);
}

int main(int argc, char **argv)
{
    if (argc < 2)
	err();
    int operand = atoi(argv[1]);
    if (operand == 0) {
	printf("%d\n", 0);	// default reasonable response to 0
	err();
    };
    printf("%.0f\n", floor(log10(operand)));
}
