#pragma once
#include <iostream>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <queue>
#include <cctype>

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
    friend fmt &operator%(fmt &&, T &&);
    friend std::ostream &operator<<(std::ostream &, const fmt &);

    struct Spec
    {
        static constexpr std::string_view TYPES = "%sr";
        char type = 'r';
        char fill = ' ';
        int width = 0;
        bool capture = false;
        bool align_left = false;
        bool sign = false;
        Spec() = default;
        Spec(char type) : type(type){};
        std::string operator()(std::queue<std::string> &) const;
    };

protected:
    std::queue<std::string>
        queue;
};

std::string fmt::Spec::operator()(std::queue<std::string> &q) const
{
    if (type == '%')
        return "%";

    std::stringstream ss;
    ss << ((sign) ? std::showpos : std::noshowpos);
    ss << ((align_left) ? std::left : std::right);

    int w;
    if (capture && !q.empty())
    {
        w = stoi(q.front());
        q.pop();
    }
    else
    {
        w = width;
    }

    ss << std::setfill(fill) << std::setw(w);
    // Currently used type has only 1 meaning;
    std::string item = q.front();
    ss << item;
    q.pop();

    return ss.str();
}

fmt::Spec _parse(std::string::iterator begin, std::string::iterator end)
{
    fmt::Spec sp(*end);
    std::string width = "";

    while (begin != end)
    {
        char c = *begin;

        if (c == ' ' || c == '0')
            sp.fill = c;
        else if (c == '-')
            sp.align_left = true;
        else if (c == '+')
            sp.sign = true;
        else if (c == '*')
            sp.capture = true;
        else if (std::isdigit(c))
            width.push_back(c);

        begin++;
    }

    if (width != "")
        sp.width = std::stoi(width);

    return sp;
}

fmt operator""_fmt(const char *str, std::size_t)
{
    return fmt{str};
}

template <class T>
fmt &operator%(fmt &&f, T &&arg)
{
    f.queue.push(repr<T>{}(arg));
    return f;
}

std::ostream &operator<<(std::ostream &os, const fmt &f)
{
    fmt tmp = f;
    for (int i = 0; i < tmp.str.length(); ++i)
    {
        if (tmp.str[i] == '%' && !tmp.queue.empty())
        {
            ++i;
            auto specbegin = tmp.str.begin() + i;
            auto specend = std::find_first_of(specbegin, tmp.str.end(), fmt::Spec::TYPES.begin(), fmt::Spec::TYPES.end());
            fmt::Spec sp;
            if (specend != tmp.str.end())
            {
                sp = _parse(specbegin, specend);
                os << sp(tmp.queue);
                i = specend - tmp.str.begin() + 1;
            }
            else
            {
                os << tmp.queue.front();
                tmp.queue.pop();
            }
        }
        os << tmp.str[i];
    }
    return os;
}

template <class... Ts>
fmt fmt::operator()(Ts... args)
{
    (this->queue.push(repr<Ts>{}(args)), ...);
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
