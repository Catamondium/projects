const readFileSync = require('fs').readFileSync;

/**
 * Derive ISO date ranges from 2-col table of DD/MM/YY dates
 * @param {String} path table of start end pairs
 * @returns {Generator<{start: String, end: String}>} ISO compliant date ranges
 */
exports.parse = function* (path) {
    function gdate(ukdate) {
        // DD/MM/YYYY parser
        const [dd, mm, yy,] = ukdate.split('/').map(x => parseInt(x, 10));
        return new Date(yy, mm-1, dd).toISOString();
    }

    data = readFileSync(path, {encoding: 'utf-8'});
    let pairs = data.split('\n');
    for (let pair of pairs) {
        com = pair.indexOf('#') - 1;
        clean = pair.slice(0, (com < 0) ? pair.length : com).trim(); // comment support
        if (clean != '') {
            const [start, end,] = clean.split(/\s/).map(gdate);
            yield {
                start: start,
                end: end
            };
        }
    }
}
