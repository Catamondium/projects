#include <math.h>
#include <stdio.h>

void calcEtime(int St[2], int t, float Et[2]) {
	int offset = St[0] * 60 + St[1];
	int tot = offset + t;
	Et[0] = floor(tot / 60);
	Et[1] = tot % 60;
	return;
	} 
		
int main(int argc, char *argv[]) {
	int elapse = 90;
	int Stime[2] = {1, 30};
	
	float Etime[2];
	calcEtime(Stime, elapse, Etime);
	
	printf("End time\t%02f:%02f\n", Etime[0], Etime[1]);
	}
