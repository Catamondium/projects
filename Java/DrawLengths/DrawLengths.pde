void setup() {
  size(400, 300);
  colorMode(HSB, height);
  rectMode(CORNER);
}

void draw() {
  background(0);
  frameRate(2);

  // Generate data
  float[] data = new float[floor(random(10, 100))];
  for (int i = 0; i < data.length; i++) {
    data[i] = random(5, 100);
  }

  // Draw
  float divisions = height / data.length;
  for (int i = 0; i < data.length; i++) {
    float len = map(data[i], 0, 100, 0, width);
    float row = map(i, 0, data.length, 0, height);
    color col = color(row, height, height);
    stroke(col);
    fill(col);
    rect(0, row, len, divisions);
  }

  //saveFrame("output.png"); 
  //noLoop();
}
