#include <math.h>
#include <stdio.h>
#include <stdlib.h>

void calcEtime(int St[2], int t, int Et[2]) {
	int offset = St[0] * 60 + St[1];
	int tot = offset + t;
	Et[0] = (int) (floor(tot / 60));
	Et[1] = (int) (tot % 60);
	return;
	} 
		
int main(int argc, char *argv[]) {
	int elapse, Stime[2], Etime[2];
	const char delimiters[] = " :";
	
	if(argc < 3) {
		return 1;
	} else {
		elapse = atoi(argv[2]);
		printf("%i\n", elapse);
		
		// TODO: split argv[1] by ':'
	}
	
	elapse = 90;
	Stime[0] = 1;
	Stime[1] = 30; // Expect 3:00 out
		
	Etime[2];
	calcEtime(Stime, elapse, Etime);
	
	printf("Start time:\t%02i:%02i", Stime[0], Stime[1]);
	printf("\t%+d\n", elapse);
	printf("End time:\t%02i:%02i\n", Etime[0], Etime[1]);
	}
