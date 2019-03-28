#pragma once
#include <iostream>
#include <sstream>
#include <iomanip>
#include <queue>

struct fmt
{
    std::string str;
    template <class T>
    fmt(const T &str) noexcept : str(str){};
    inline std::string operator()() noexcept;
    template <class T, class... Ts>
    std::string operator()(T, Ts...) noexcept;
    template <class T>
    fmt &operator+=(const T &) noexcept;
    template <class T>
    fmt operator+(const T &) const noexcept;
    operator std::string() const { return str; };
};

template <class T>
fmt &fmt::operator+=(const T &rhs) noexcept
{
    this->str += std::string(rhs);
    return *this;
}

template <class T>
fmt fmt::operator+(const T &rhs) const noexcept
{
    return fmt(*this) += std::string(rhs);
}

fmt operator""_fmt(const char *str, std::size_t) noexcept
{
    return {str};
}

inline std::string fmt::operator()() noexcept
{
    return str;
}

void _collect(std::queue<std::string> &queue)
{
    // noop
}

template <class T, class... Ts>
void _collect(std::queue<std::string> &queue, T &val, Ts &... rest)
{
    queue.push(repr<T>{}(val));
    _collect(queue, rest...);
}

std::string _print(std::queue<std::string> &queue, std::string str)
{
    std::stringstream out;
    for (int i = 0; i < str.length(); ++i)
    {
        if (str[i] == '%')
        {
            ++i;
            if (str[i] != '%' && !queue.empty())
            {
                out << queue.front();
                queue.pop();
            }
        }
        out << str[i];
    }
    return out.str();
}

template <class T, class... Ts>
inline std::string fmt::operator()(T val, Ts... rest) noexcept
{
    std::queue<std::string> queue;
    _collect(queue, val, rest...);
    return _print(queue, str);
}

// Emulate std::hash<T>{}(thing) interface
template <class T>
struct repr
{
    std::string operator()(T in)
    {
        std::stringstream ss;
        ss << in;
        return ss.str();
    }
};

template <>
struct repr<bool>
{
    std::string operator()(bool bol)
    {
        std::stringstream ss;
        ss << std::boolalpha << bol;
        return ss.str();
    }
};

// ~printf family
namespace fmtf
{
// sprintf
template <class... Ts>
std::string string(std::string format, Ts... rest)
{
    return fmt(format)(rest...);
}

// fprintf, where 'f' is some std::ostream
template <class... Ts>
void fprint(std::ostream &stream, std::string format, Ts... rest)
{
    stream << fmt(format)(rest...);
}

// fprintf to cout/stdout
template <class... Ts>
void print(std::string format, Ts... rest)
{
    std::cout << fmt(format)(rest...);
}
} // namespace fmtf
