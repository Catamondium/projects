#include <iostream>
#include <sstream>
#include <iomanip>
#include <vector>
#include "format.h"

struct S {
	int x;
	S(int x): x(x) {};
};

// User specialisation
template<>
struct repr<S> {
	std::string operator()(S n)
	{
		std::stringstream ss;
		ss << std::showpos << "S(" << n.x << ")";
		return ss.str();
	}
};

template<class X>
struct repr<std::vector<X>> {
	std::string operator()(std::vector<X> vec)
	{
		if(vec.size() == 0) return "[]";
		else {
			std::string ret = "[";
			for(X &val : vec) {
				ret += repr<X>{}(val) + ", ";
			}
			return ret.substr(0, ret.size()-2) + "]";
		}
	}
};

int main()
{
	std::cout << "_fmt: %"_fmt(5) << std::endl;
	std::cout << "bol: % %"_fmt(true, false, 555) << std::endl;
	std::cout << "str: %"_fmt(std::string("string")) << std::endl;
	std::cout << "escaped: %% continued"_fmt(44444) << std::endl;

	// Stringable, accepts any T -> std::string
	fmt addtest = "Ass";
	addtest += "Bass";
	std::cout << "addtest: " << addtest() << std::endl;

	std::vector ints = {0, 1, 2, 3, 4, 5};
	std::cout << "%"_fmt(ints) << std::endl;

	std::cout << "%"_fmt(S(600)) << std::endl;

	fmtf::print("Printf: %\n", true);
}
