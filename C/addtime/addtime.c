#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void calcEtime(int St[2], int t, int Et[2]) {
	int offset = St[0] * 60 + St[1];
	int tot = offset + t;
	Et[0] = (int) (floor(tot / 60));
	Et[1] = (int) (tot % 60);
	return;
	} 
		
int main(int argc, char *argv[]) {
	int Stime[2], Etime[2];
	signed int elapse;
	
	if(argc < 3) {
		printf("Error\tmm:hh mins expected.");
		return 1;
	} else {
		elapse = atoi(argv[2]);
		// Split and collect start time
		const char delimiters[] = " :";
		char in[60];
		strcpy(in, argv[1]);
		char * hrs;
		char * mins;
		hrs = strtok(in, delimiters);
		mins = strtok(NULL, delimiters);
		Stime[0] = atoi(hrs);
		Stime[1] = atoi(mins);
	}
		
	calcEtime(Stime, elapse, Etime);
	
	printf("Start time:\t%02i:%02i", Stime[0], Stime[1]);
	printf("\t%+d\n", elapse);
	printf("End time:\t%02i:%02i\n", Etime[0], Etime[1]);
	}
