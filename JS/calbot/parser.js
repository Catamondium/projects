const readFile = require('fs').readFile;
const path = 'test';

function gdate(ukdate) {
	// DD/MM/YYYY parser
	let segs = ukdate.split('/');
	segs = segs.map(n => parseInt(n));
	return new Date(segs[2], segs[1]-1, segs[0]).toISOString();
}

/* push fails, objects form successfully.
 * suspected scoping issue
 * splice appending fails to
 * using a Set has same effect
 */
function parse(file) {
	let ret = new Array();
	let outervar = 25; // XXX scopetest
	readFile(file, 'utf8', (err, data) => {
		if (err) throw err;
		let pairs = data.split('\n');
		pairs.forEach((pair) => {
			if (pair != '') {
				let dates = pair.split(' ');
				ret.push({
					start: gdate(dates[0]),
					end: gdate(dates[1])
				});
				console.log(outervar); // Works?
				ret.push('objpush'); // XXX fail
			}
		})
		ret.push('lamda'); // XXX fail
	})
	ret.push('declscope'); // XXX success
	return ret;
}

console.log(parse(path));
