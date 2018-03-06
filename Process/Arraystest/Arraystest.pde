void setup() {
  size(300, 600);
  colorMode(HSB, height);
}

void draw() {
  noLoop();
  background(0);

  int[] data = {50, 82, 20, 65, 45, 80, 90, 100, 95, 50};
  float divisions = height / data.length;

  for (int i = 0; i < data.length; i++) {
    fill(i * divisions, 255, 255);
    rect(0, i * divisions, data[i], divisions);
  }
  
  saveFrame("output.png");
}