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
 *
 * TODO
 * * colour input
 * * * turtle
 * * * stroke
 */

function setup() {
	win = createCanvas(200, 200);
	select('#window').child(win);
	angleMode(DEGREES);
	turtle = new Turtle(100, 100, -90, '#00FF00');
	editor = select('#code');
	editor.input(run);
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
