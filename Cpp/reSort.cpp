#include <iostream>
#include <regex>
#include <unistd.h>

/* Groups given strings based on regex match and prints them
 */

void usage(const std::string prog)
{
	std::cout <<
		"usage: " << prog << " [-ipageh] <regex> <strings>*\n"
		"options:\n"
		"\t-i invert group order\n"
		"\t-c case-insensitive\n"
		"\t-e ECMAScript regex (default)\n"
		"\t-p POSIX regex\n"
		"\t-a AWK regex\n"
		"\t-g GREP regex\n"
		"\t-h print this usage" << std::endl;
	std::exit(1);
}

int main(int argc, char **argv)
{
	bool invert = false, match_case = true;
	auto mode = std::regex::ECMAScript;

	int c;
	while((c = getopt(argc, argv, "icpageh")) != -1) {
		switch(c) {
			case 'i':
				invert = true;
				break;
			case 'c':
				match_case = false;
			case 'p':
				mode = std::regex::basic;
				break;
			case 'a':
				mode = std::regex::awk;
				break;
			case 'g':
				mode = std::regex::grep;
				break;
			case 'e':
				mode = std::regex::ECMAScript;
				break;
			default:
				usage(argv[0]);
		}
	}

	if(!match_case) mode |= std::regex::icase;

	std::regex re;
	std::vector<std::string> matched, unmatched;

	if((argc - optind) < 1) usage(argv[0]);

	try {
		re = std::regex(argv[optind++], mode);
	} catch (const std::regex_error& e) {
		std::cout << e.what() << std::endl;
		std::exit(1);
	}

	for(int i = optind; i < argc; ++i) {
		std::cmatch m;
		if(regex_match(argv[i], m, re))
			matched.push_back(argv[i]);
		else
			unmatched.push_back(argv[i]);
	}

	if(invert) matched.swap(unmatched);

	for(auto &str : matched)
		std::cout << str << std::endl;
	for(auto &str : unmatched)
		std::cout << str << std::endl;
}
