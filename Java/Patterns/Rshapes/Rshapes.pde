void setup() {
  size(700, 600);
  colorMode(HSB, 360, 1, 1);
}

void draw() {
  //background(0);
  frameRate(2);
  randomQuad(width, height);
}

void randomPoly(int w, int h, int v) {
  beginShape();

  stroke(random(0, 360), 1, 1);
  fill(random(0, 360), 1, 1);
  strokeWeight(3);

  for (int i = 0; i < v; i++) {
    vertex(random(0, w), random(0, h));
  }

  endShape(CLOSE);
}

void randomQuad(int w, int h) {
  randomPoly(w, h, 4);
}
