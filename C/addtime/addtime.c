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
	int Stime[2] = {1, 30}; // Expect 3:00 out
	
	float Etime[2];
	calcEtime(Stime, elapse, Etime);
	
	char Ostr_1[15];
	sprintf(Ostr_1, "Start time: %02.0f:%02.0f", Stime[0], Stime[1]);
	char Ostr_2[15];
	sprintf(Ostr_2, "End time: %02.0f:%02.0f\n", Etime[0], Etime[1]);
	
	printf("%s %+i\n%s", Ostr_1, elapse, Ostr_2);
	}
