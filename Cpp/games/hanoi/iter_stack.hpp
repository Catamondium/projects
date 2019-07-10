#pragma once
#include <stack>
#include <deque>

template <class T, class Container = std::deque<T>>
struct Iter_stack : public std::stack<T, Container>
{
    using iterator = typename Container::iterator;
    using reverse_iterator = typename Container::reverse_iterator;
    using const_iterator = typename Container::const_iterator;
    using const_reverse_iterator = typename Container::const_reverse_iterator;
    T operator[](size_t i)
    {
        return this->c[i];
    }

    iterator begin() { return this->c.begin(); }
    iterator end() { return this->c.end(); }
    const_iterator cbegin() const { return this->c.cbegin(); }
    const_iterator cend() const { return this->c.cend(); }

    reverse_iterator rbegin() { return this->c.rbegin(); }
    reverse_iterator rend() { return this->c.rend(); }
    const_reverse_iterator crbegin() { return this->c.crbegin(); }
    const_reverse_iterator crend() const { return this->c.crend(); }

    Container cont() { return this->c; }
};