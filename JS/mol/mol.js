#!/usr/bin/env node

const coeffRe = /^(\d+)/g // beginning coefficient
const tokRe = /\(.*?\)|([A-Z][a-z]*)(\d*)/g  // groups: Elem, [coeff]
const subRe = /\((.*)\)(\d*)/g // groups: expr, [coeff]

function makeCoeff(c) {
    if (c == '' || c === undefined) {
        return 1;
    } else {
        return parseInt(c);
    }
}

ptable = {
    H: 1,
    C: 12,
    undefined: 0 // undefined is keyable?
}

// if(__name__ == '__main__') eqiv, supporting web embedding
if (typeof require != 'undefined' && require.main == module) {
    test = "CH3(CH3)2CH3"
    while ((m = tokRe.exec(test)) !== null) {
        groups = m.slice(1);
        console.log(`E:\t${ptable[groups[0]]}\tCoeff:\t${makeCoeff(groups[1])}`);
    }

    while ((m = subRe.exec(test)) !== null) {
        groups = m.slice(1);
        console.log(`Exp:\t${ptable[groups[0]]} Coeff:\t${makeCoeff(groups[1])}`);
    }
}