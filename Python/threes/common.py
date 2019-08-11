#!/usr/bin/env python3

REMOTE = ('', 22555)
NIX = '/tmp/threes_sock'


def window(seq, n=2):
    import itertools
    """
    Returns a sliding window (of width n) over data from the iterable
    s -> (s0,s1,...s[n-1]), (s1,s2,...,sn), ...
    https://stackoverflow.com/a/6822773/9664844
    """
    it = iter(seq)
    result = tuple(itertools.islice(it, n))
    if len(result) == n:
        yield result
    for elem in it:
        result = result[1:] + (elem,)
        yield result


"""
App level handling api?
        dict[funcs] -> Callable
        call_iter -> name, *args
        peripheral data -> **kwargs
        dict[name](*args, **kwargs)
"""


def call_iter(it, sep=' '):
    """
    Adapt it to yield (name, *args) by word splitting
    Output is stripped of whitespace
    """
    for i in it:
        head, *tail = i.strip().split(sep)
        yield (head, tail)
