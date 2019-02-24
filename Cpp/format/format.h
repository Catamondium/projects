// g++-8 -std=c++2a -fconcepts
#pragma once
#include <iostream>
#include <sstream>
#include <iomanip>

template<class T>
concept bool Stringable = requires(T a) {
	{std::string(a)} -> std::string;
};
static_assert(! Stringable<int>, "Ints are strings!");

struct fmt {
	std::string str;
	fmt(const Stringable &str) noexcept: str(str){};
	inline std::string operator()() noexcept;
	template<class T, class... Ts> std::string operator()(T, Ts...) noexcept;
	fmt& operator+=(const Stringable&) noexcept;
	fmt operator+(const Stringable&) const noexcept;
	operator std::string() const { return str; }
};

fmt& fmt::operator+=(const Stringable &rhs) noexcept
{
	this->str += std::string(rhs);
	return *this;
}

fmt fmt::operator+(const Stringable &rhs) const noexcept
{
	return fmt(*this) += std::string(rhs);
}

fmt operator""_fmt(const char * str, std::size_t) noexcept
{
	return {str};
}

// Emulate std::hash<T>{}(thing) interface
template<class T>
struct repr {
	std::string operator()(T in)
	{
		std::stringstream ss;
		ss << in;
		return ss.str();
	}
};

template<>
struct repr<bool> {
	std::string operator()(bool bol)
	{
		std::stringstream ss;
		ss << std::boolalpha << bol;
		return ss.str();
	}
};

void print_(std::stringstream &out, auto &start, auto &end)
{
	for(auto it = start; it != end; ++it) {
		out << *it;
	}
}

template<class T, class... Ts>
void print_(std::stringstream &out, auto start, auto end, T &val, Ts&... args)
{
	for(auto it = start; it != end; ++it) {
		if(*it == '%') {
			++it;
			if(*it != '%') {
				out << repr<T>{}(val);
				print_(out, it, end, args...); // enumerate recursively
				break;
			}
		}
		out << *it;
	}
}

// ~printf family
namespace fmtf {
	// sprintf
	template<class... Ts>
	std::string string(std::string format, Ts... args)
	{
		return fmt(format)(args...);
	}

	// fprintf, where 'f' is some std::ostream
	template<class... Ts>
	void fprint(std::ostream& stream, std::string format, Ts... args)
	{
		stream << fmt(format)(args...);
	}

	// fprintf to cout/stdout
	template<class... Ts>
	void print(std::string format, Ts... args)
	{
		std::cout << fmt(format)(args...);
	}
}

inline std::string fmt::operator()() noexcept
{
	return str;
}

template<class T, class... Ts>
inline std::string fmt::operator()(T val, Ts... args) noexcept
{
	std::stringstream out;
	print_(out, str.begin(), str.end(), val, args...);
	return out.str();
}
