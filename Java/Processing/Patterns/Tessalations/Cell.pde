class Segment {
  PVector pos;
  float l, w, hue;

  Segment(PVector pos_, float len_, float w_, float hue_) {
    pos = pos_;
    l = len_;
    w = w_;
    hue = hue_;
  }

  void show() {
    pushStyle();
    rectMode(CORNER);
    colorMode(HSB, TAU);
    fill(hue, TAU, TAU);
    stroke(hue, TAU, TAU);
    rect(pos.x, pos.y, l, w);
    popStyle();
  }
}

class Cell {
  PVector pos;
  float w, h;
  float[] data;
  Segment[] celldata;

  Cell(PVector pos_, float w_, float h_) {
    pos = pos_;
    w = w_;
    h = h_;
  }

  void gen() {
    data = new float[floor(random(10, 100))];
    for (int i = 0; i < data.length; i++) {
      data[i] = random(5, 100);
    }
  }

  void update() {
    gen();
    celldata = new Segment[data.length];
    float hei = h / data.length;

    for (int i = 0; i < data.length; i++) {
      float len = map(data[i], 0, 100, 0, w);
      float seg_y = map(i, 0, data.length, pos.y - 0.5 * h, pos.y + 0.5 * h);
      float hue = map(i, 0, data.length, 0, TAU);
      PVector seg_pos = new PVector(pos.x - 0.5 * w, seg_y);
      celldata[i] = new Segment(seg_pos, len, hei, hue);
    }
  }

  void show() {
    pushStyle();
    for (Segment a : celldata) {
      a.show();
    }

    rectMode(CENTER);
    noFill();
    colorMode(RGB, 255);
    stroke(255);
    rect(pos.x, pos.y, w, h);
    popStyle();
  }
}
