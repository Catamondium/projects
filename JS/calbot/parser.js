const readFileSync = require('fs').readFileSync;

exports.gdate = function (ukdate) {
    // DD/MM/YYYY parser
    let segs = ukdate.split('/');
    segs = segs.map(n => parseInt(n));
    return new Date(segs[2], segs[1] - 1, segs[0]).toISOString();
}

exports.parse = function (file) {
    let ret = new Array();
    data = readFileSync(file).toString();
    let pairs = data.split('\n');
    pairs.forEach((pair) => {
        if (pair != '') {
            let dates = pair.split('\t');
            ret.push({
                start: exports.gdate(dates[0]),
                end: exports.gdate(dates[1])
            });
        }
    })
    return ret;
}
