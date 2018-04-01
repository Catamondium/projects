class Container {
  float x, y, w, h, divisions;
  float[] data;

  Container(float x_, float y_, float w_, float h_) {
    x = x_;
    y = y_;
    w = w_;
    h = h_;
  }

  void update() {
    data = new float[floor(random(10, 100))];
    for (int i = 0; i < data.length; i++) {
      data[i] = random(5, 100);
    }
    divisions = h / data.length;
  }

  void show() {
    pushStyle();
    pushMatrix();
    translate(x, y);
    colorMode(HSB, h);
    rectMode(CORNER);
    for (int i = 0; i < data.length; i++) {
      float len = map(data[i], 0, 100, 0, w);
      float row = map(i, 0, data.length, 0, h);
      color col = color(row, h, h);
      stroke(col);
      fill(col);
      rect(0, row, len, divisions);
    }
    colorMode(RGB, 255);
    noFill();
    stroke(255);
    rect(0, 0, w, h);
    popMatrix();
    popStyle();
  }
}
