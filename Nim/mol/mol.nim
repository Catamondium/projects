import
    parsecsv,
    streams,
    tables,
    strtabs,
    strformat,
    strutils,
    re,
    parseopt



proc loadtable(loc: string): Table[string, float] =
    #let str = staticRead(loc)
    var
        strm = staticRead(loc).newStringStream
        p: CsvParser
    p.open(strm, "tmp.tsv", separator = '\t')
    p.readHeaderRow()
    while p.readRow():
        let
            sym = p.rowEntry("Elem")
            mr = p.rowEntry("Mr").parseFloat
        assert(sym notin result)
        result[sym] = mr

const ptable = static(loadtable("ptable.tsv"))



proc valid(c: char): bool =
    c.isAlphaNumeric or c in "()"

proc sanitise(src: string): string =
    let ttable = static(newStringTable("{", "(", "[", "(", "]", ")", "}", ")", modeCaseSensitive))
    result = newStringOfCap(src.len)
    for ch in src:
        let nch = ttable.getOrDefault($ch, $ch)
        if nch[0].valid:
            result.add($nch)

proc mass(comp: string): float =
    let
        coeffre = re"^(\d+)" # overall coefficient
        elem = "([A-Z][a-z]*)" # element identifier
        tokre = re(fmt"\(.*?\)|{elem}(\d*)") # a given 'token'
        subre = re"\((.*)\)(\d*)" # a subexpression
    
    var
        bigcoeff = 1
        acc = 0.0
    
    if comp =~ coeffre:
        bigcoeff = try: matches[0].parseInt except: 1
    
    #for m in comp.findAll(elem):
    #    echo fmt"elem: {m}"

    for m in comp.findAll(tokre):
        if m =~ tokre:
            if matches[0] == "":
                continue
            let
                coeff = try: matches[1].parseInt except: 1
                mr = ptable[matches[0]]
            acc += float(coeff) * mr
            echo fmt"tokre: {matches}"
    
    for m in comp.findAll(subre):
        if m =~ subre:
            if matches[0] == "":
                continue
            let
                coeff = try: matches[1].parseInt except: 1
                mr = mass(matches[0])
            acc += float(coeff) * mr
            echo fmt"subre: {matches}"
    float(bigcoeff) * acc

var p = initOptParser()
try:
    for kind, key, _ in p.getopt:
        if kind == cmdArgument:
            let sanitised = sanitise(key)
            echo fmt"{sanitised}: {mass(sanitised):.3}"
except KeyError as e:
    echo fmt"Error: element {e.msg.substr(15)} doesn't exist"