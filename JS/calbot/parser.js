const readFileSync = require('fs').readFileSync;

exports.gdate = function (ukdate) {
    // DD/MM/YYYY parser
    const [dd, mm, yy] = ukdate.split('/');
    return new Date(yy, mm, dd).toISOString();
}

exports.parse = function (file) {
    let ret = new Array();
    data = readFileSync(file, {encoding: 'utf-8'});
    let pairs = data.split('\n');
    pairs.forEach((pair) => {
        pair = pair.slice(0, pair.indexOf('#') - 1).trim() // comment support
        if (pair != '') {
            const [start, end,] = pair.split(/\s/).map(exports.gdate);
            ret.push({
                start: start,
                end: end
            });
        }
    })
    return ret;
}
