#pragma once
#include <stack>
#include <deque>

template <class T, class Container = std::deque<T>>
struct Iter_stack : public std::stack<T, Container>
{
    using iterator = typename Container::iterator;
    using const_iterator = typename Container::const_iterator;
    T operator[](size_t i)
    {
        return this->c[i];
    }

    iterator begin() { return this->c.begin(); }
    iterator end() { return this->c.end(); }
    const_iterator cbegin() const { return this->c.cbegin(); }
    const_iterator cend() const { return this->c.cend(); }

    Container cont() { return this->c; }
};