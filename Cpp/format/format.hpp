#pragma once
#include <iostream>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <queue>
#include <cctype>
#include <optional>

template <class T>
struct repr
{
    std::string visit(T in)
    {
        std::stringstream ss;
        ss << in;
        return ss.str();
    }
};

template <>
struct repr<bool>
{
    std::string visit(bool bol)
    {
        std::stringstream ss;
        ss << std::boolalpha << bol;
        return ss.str();
    }
};

struct fmt
{
    std::string format;
    fmt(const std::string &format) : format(format){};
    fmt(const char *format) : format(format){};
    fmt(fmt &) = default;
    fmt(const fmt &) = default;
    template <class... Ts>
    fmt operator()(Ts...);
    operator std::string() const { return format; };
    friend std::ostream &operator<<(std::ostream &, const fmt &);

protected:
    std::optional<std::string> result;
};


fmt operator""_fmt(const char *str, std::size_t)
{
    return fmt{str};
}

std::ostream &operator<<(std::ostream &os, const fmt &f)
{
    os << *(f.result); // deliberately error prone
    return os;
}

template <class... Ts>
fmt fmt::operator()(Ts... args)
{
    size_t max = format.length();
    std::string out;
    out.reserve(max); // prevent overdone allocations

    size_t i = 0; // leave iterators for now
    while (i < max) {
        if (format[i] == '{') {
            if (i+1 < max-1) {
                if (format[i+1] == '{') {
                    out += '{';
                    i += 2;
                    continue;
                }
            }

            auto oi = format.find('}', i);
            if (oi == std::string::npos) {
                throw std::logic_error("Unterminated format");
            }

            std::string pattern = format.substr(i+1, oi-i-1);
            out += "<sub '" + pattern + "'>";
            i = oi + 1;
        } else if (format[i] == '}') {
            out += '}';
            i += 2;
        } else {
            out += format[i];
            i += 1;
        }
    }
    result = out;
    return *this;
}

// ~printf family
namespace fmtf
{
// sprintf
template <class... Ts>
std::string string(std::string format, Ts... args)
{
    return fmt{format}(args...);
}

// fprintf, where 'f' is some std::ostream
template <class... Ts>
void fprint(std::ostream &stream, std::string format, Ts... args)
{
    stream << fmt{format}(args...);
}

// fprintf to cout/stdout
template <class... Ts>
void print(std::string format, Ts... args)
{
    std::cout << fmt{format}(args...);
}
} // namespace fmtf
