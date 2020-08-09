const readFileSync = require('fs').readFileSync;

exports.gdate = function (ukdate) {
    // DD/MM/YYYY parser
    const [dd, mm, yy] = ukdate.split('/').map(x => parseInt(x, 10));
    return new Date(yy, mm-1, dd).toISOString();
}

exports.parse = function* (file) {
    data = readFileSync(file, {encoding: 'utf-8'});
    let pairs = data.split('\n');
    for (let pair of pairs) {
        com = pair.indexOf('#') - 1;
        clean = pair.slice(0, (com < 0) ? pair.length : com).trim(); // comment support
        if (clean != '') {
            const [start, end,] = clean.split(/\s/).map(exports.gdate);
            yield {
                start: start,
                end: end
            };
        }
    }
}
