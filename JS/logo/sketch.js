let editor;
let turtle;

/* Logo commands
 * -----------------
 * forward/backward        fd bk
 * pen up/down             pu pd
 * left/right rotation     lt rt
 * hide/show turtle        ht st
 * repeat int { expr }
 * reset
 */

function setup() {
	angleMode(DEGREES);

	win = createCanvas(200, 200);
	select('#window').child(win);
	editor = select('#code');
	lcol = select('#lcol')
	tcol = select('#tcol');

	lcol.input(spawn);
	tcol.input(spawn);
	editor.input(run);

	spawn()
	run();
}

function spawn() {
	turtle = new Turtle(100, 100, -90, tcol.value(), lcol.value());
	run()
}

function run() {
	push();
	background(0);
	let code = editor.value();
	let tokens = code.split(' ');
	eval(tokens);
	turtle.render();
	pop();
}
