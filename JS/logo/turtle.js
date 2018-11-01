class Turtle {
	constructor(x, y, theta, col) {
		translate(x, y);
		rotate(theta);
		this.col = color(col);
		this.pen = true;
		this.show = true;
	}

	forward(len) {
		if(this.pen) {
			stroke(255);
			strokeWeight(2);
			line(0, 0, len, 0);
		}
		translate(len, 0);
	}

	right(theta) {
		rotate(theta);
	}

	render() {
		if (this.show) {
			push()
			stroke(this.col);
			noStroke();
			fill(this.col);
			triangle(0, -10, 0, 10, 5, 0);
			pop()
		}
	}
}

/* Logo commands
 * -----------------
 * forward/backward        fd bk
 * pen up/down             pu pd
 * left/right rotation     lt rt
 * hide/show turtle        ht st
 * repeat int [ expr ]
 * reset
 */
