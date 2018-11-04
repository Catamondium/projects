class Turtle {
	constructor(x, y, theta, tcol, strk) {
		translate(x, y);
		rotate(theta);
		this.col = color(tcol);
		this.lin = color(strk);
		this.pen = true;
		this.show = true;
	}

	forward(len) {
		if(this.pen) {
			stroke(this.lin);
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
			noStroke();
			fill(this.col);
			triangle(0, -10, 0, 10, 5, 0);
			pop()
		}
	}
}
