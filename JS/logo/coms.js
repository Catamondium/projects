const commands = {
	'fd': toks => {
		let len = parseFloat(toks.shift());
		turtle.forward(len);
		return toks;
		},

	'bk': toks => {
		let len = parseFloat(toks.shift());
		turtle.forward(-len);
		return toks;
		},

	'rt': toks => {
		let theta = parseFloat(toks.shift());
		turtle.right(theta);
		return toks;
		},

	'lt': toks => {
		let theta = parseFloat(toks.shift());
		turtle.right(-theta);
		return toks;
		},

	'pu': toks => {
		turtle.pen = false;
		return toks;
		},

	'pd': toks => {
		turtle.pen = true;
		return toks;
		},

	'ht': toks => {
		turtle.show = false;
		return toks;
	},

	'st': toks => {
		turtle.show = true;
		return toks;
	},
	
	'repeat': toks => {
		let iters = parseInt(toks.shift());
		let end = toks.indexOf('}')
		let interior = toks.slice(1, end);

		for (let i = 0; i < iters; i++) {
			eval(interior.slice());
		}

		return toks.slice(end + 1);
	},

	'reset': toks => {
		turtle.show = true;
		turtle.pen = true;
		return toks;
	}
}

function eval(tokens) {
	while(tokens.length != 0) {
		let com = tokens.shift();
		tokens = commands[com](tokens);
	}
}
