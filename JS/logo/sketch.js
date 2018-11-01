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
	createCanvas(200, 200);
	angleMode(DEGREES);
	turtle = new Turtle(100, 100, 0, '#FF0000');
	editor = select('#code');
	run();
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
