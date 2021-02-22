#!/usr/bin/python

def error_fn(target):
    """
    KMP error function preproc
    no -1 flags, 0 based jumping directly
    """
    ret = [0] * (len(target)+1)
    pos = 1
    cnd = 0

    while pos < len(target):
        while pos < len(target) and target[cnd] != target[pos]:
            # zip suffix cursor forward
            pos += 1

        if pos < len(target):
            # true match
            ret[pos] = cnd
            # shift both cursors
            # hope for consecutive matches
            cnd += 1
            pos += 1
    return ret

def KMP(search, target):
    """
    Find indices of target in search
    runs in O(m+k) ~ O(n)
    """
    ret = list()
    T = error_fn(target)
    si = 0
    ti = 0
    while si < len(search):
        if target[ti] == search[si]:
            # match
            ti += 1
            si += 1
            if ti == len(target):
                ret.append(si - ti)
                ti = T[ti] # ti = 0
        else:
            ti = T[ti] # partial reset
            si += 1 # ti is on next symbol
    return ret

w = "abcabcasbcz"
s = w + w[:-8] + w + "ubvutbv" + w + w[4:] + w
print(f"{w}\nin {s}\nat {KMP(s, w)}")
print(f"Error fn: {error_fn(w)}")