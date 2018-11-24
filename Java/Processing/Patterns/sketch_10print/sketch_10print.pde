void setup() {
  size(600, 600);
}

void draw() {
  background(0);
  frameRate(2);
  gen(20, 20, 0.5);
  //noLoop();
  //save("output.png");
}

void gen(float gridX, float gridY, float bound) {
  pushStyle();
  colorMode(HSB, width + height);
  strokeWeight(2);
  for (float col = 0; col < width; col += gridX) {
    for (float row = 0; row < height; row += gridY) {
      float num = random(1);
      stroke(col + row, width + height, width + height);
      if (num < bound) { // '/'
        line(col, row + gridY, col + gridX, row);
      } else { // '\'
        line(col, row, col + gridX, row + gridY);
      }
    }
  }
  popStyle();
}
