#include <iostream>
#include <cstdlib>

unsigned long fibonacci(unsigned int n) {
	if(n == 0 || n == 1)
		return n;
	else {
		unsigned long first = 0;
		unsigned long second = 1;
		unsigned long result;
		for(unsigned int i = 1; i < n; ++i) {
			result = first + second;
			first = second;
			second = result;
		}
		return result;
	}
}

int main(int argc, char **argv) {
	if(argc > 1) {
			std::cout.imbue(std::locale(""));
		for(unsigned int i = 0; i < atoi(argv[1]); ++i) {
				std::cout << fibonacci(i) << std::endl;
		}
		return 0;
	} else{
			std::cout << "Integer argument required\n";
		return 1;
	}
}
