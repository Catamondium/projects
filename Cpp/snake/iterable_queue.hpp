#pragma once
#include <queue>
#include <deque>

template <class T, class Container = std::deque<T>>
class iterable_queue : public std::queue<T, Container>
{
public:
    using iterator = typename Container::iterator;
    using const_iterator = typename Container::const_iterator;

    iterator begin() { return this->c.begin(); }
    iterator end() { return this->c.end(); }
    const_iterator begin() const { return this->c.begin(); }
    const_iterator end() const { return this->c.end(); }
};