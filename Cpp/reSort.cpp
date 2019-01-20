#include <iostream>
#include <regex>
#include <unistd.h>

/* Groups given strings based on regex match and prints them
 */

void usage(const std::string prog)
{
	std::cout <<
		"usage: " << prog << " [-ir:] <regex> <strings>*\n"
		"options:\n\t-i invert group order" << std::endl;
	std::exit(1);
}

int main(int argc, char **argv)
{
	bool invert = false;
	std::regex re;

	int c;
	while((c = getopt(argc, argv, "ih")) != -1) {
		switch(c) {
			case 'i':
				invert = true;
				break;
			default:
				usage(argv[0]);
		}
	}

	std::vector<std::string> matched;
	std::vector<std::string> unmatched;

	if((argc - optind) < 1) usage(argv[0]);
	else
		re = argv[optind++];

	for(int i = optind; i < argc; ++i) {
		std::cmatch m;
		if(regex_match(argv[i], m, re))
			matched.push_back(argv[i]);
		else
			unmatched.push_back(argv[i]);
	}

	if(!invert) {
		for(auto &str : matched)
			std::cout << str << std::endl;
		for(auto &str : unmatched)
			std::cout << str << std::endl;
	} else {
		for(auto &str : unmatched)
			std::cout << str << std::endl;
		for(auto &str : matched)
			std::cout << str << std::endl;
	}
}
