int main(int argc, char *argv[]) {
	int elapse = 90;
	int Stime[2] = {1, 30};
	
	int Etime[2];
	calcEtime(Stime, elapse, Etime);
	
	printf(Etime);
	}

	void calcEtime(int St[2], int t, int Et[2]) {
		int offset = St[0] * 60 + St[1];
		int tot = offset + t;
		Et = {floor(tot / 60), tot % 60};
		return;
		} 
