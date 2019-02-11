#include <iostream>
#include <regex>
#include <algorithm> // partition
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
		"\t-h print this usage\n"
		"\tregex grammars:\n"
		"\t\t-j JS / ECMAScript (default)\n"
		"\t\t-p POSIX\n"
		"\t\t-e POSIX extended\n"
		"\t\t-g Grep\n"
		"\t\t-x EGrep" << std::endl;
	std::exit(1);
}

int main(int argc, char **argv)
{
	using re = std::regex;
	bool invert = false, match_case = true;
	auto mode = re::ECMAScript;

	int c;
	while((c = getopt(argc, argv, "ichjpegx")) != -1) {
		switch(c) {
			case 'i':
				invert = true;
				break;
			case 'c':
				match_case = false;
			case 'j':
				mode = re::ECMAScript;
				break;
			case 'p':
				mode = re::basic;
				break;
			case 'e':
				mode = re::awk;
				break;
			case 'g':
				mode = re::grep;
				break;
			case 'x':
				mode = re::egrep;
				break;
			default:
				usage(argv[0]);
		}
	}

	if(!match_case) mode |= re::icase;
	mode |= re::nosubs | re::optimize; // Optimise for detection

	re pattern;
	std::vector<std::string> matched, unmatched;

	if((argc - optind) < 1) usage(argv[0]);

	try {
		pattern = re(argv[optind++], mode);
	} catch (const std::regex_error& e) {
		std::cout << e.what() << std::endl;
		std::exit(1);
	}

	/*
	auto result = std::partition(argv + optind, argv + argc,
			[pattern](auto e){
			std::cmatch m;
			return regex_match(e, m, pattern);
			});
	
	// argc - optind == len(result)
	for(int i = 0; i < (argc - optind); ++i) {
		std::cout << result[i] << std::endl;
	}
	*/

	 for(int i = optind; i < argc; ++i) {
		std::cmatch m;
		if(regex_match(argv[i], m, pattern))
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
