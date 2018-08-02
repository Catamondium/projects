#include <iostream>

using namespace std;

unsigned long fibonacci(unsigned int n) {
	switch(n) {
		case 0:
			return 0;
		case 1:
			return 1;
		default:
			unsigned long first = 0;
			unsigned long second = 1;
			unsigned long result;
			for(unsigned int i = 1; i < n; i++) {
				result = first + second;
				first = second;
				second = result;
			}
			return result;
	}
}

int main(int argc, char** argv) {
	cout.imbue(locale(""));
	for(unsigned int i = 0; i < 10; i++) {
		cout << fibonacci(i) << endl;
	}
	return 0;
}
