#pragma once
#include <iostream>
#include <sstream>
#include <iomanip>
#include <queue>

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

struct fmt
{
    std::string str;
    fmt(const std::string &str) : str(str){};
    fmt(const char *str) : str(str){};
    fmt(fmt &) = default;
    fmt(const fmt &) = default;
    template <class... Ts>
    fmt operator()(Ts...);
    operator std::string() const { return str; };
    template <class T>
    friend fmt operator%(fmt, T);
    friend std::ostream &operator<<(std::ostream &, const fmt &);

protected:
    std::queue<std::string> queue;
};

fmt operator""_fmt(const char *str, std::size_t)
{
    return {str};
}

template <class T>
fmt operator%(fmt f, T arg)
{
    f.queue.push(repr<T>{}(arg));
    return f;
}

std::ostream &operator<<(std::ostream &os, const fmt &f)
{
    fmt tmp = f;
    for (int i = 0; i < tmp.str.length(); ++i)
    {
        if (tmp.str[i] == '%')
        {
            ++i;
            if (tmp.str[i] != '%' && !tmp.queue.empty())
            {
                os << tmp.queue.front();
                tmp.queue.pop();
            }
        }
        os << tmp.str[i];
    }
    return os;
}

namespace
{
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
} // namespace

template <class... Ts>
fmt fmt::operator()(Ts... rest)
{
    _collect(queue, rest...);
    return *this;
}

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
