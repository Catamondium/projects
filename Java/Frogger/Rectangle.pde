class Rectangle {
  float x, y, w, h;
  color col = #000000;

  Rectangle(float x_, float y_, // Constructed relative to top-left
    float w_, float h_, color col_) {
    x = x_;
    y = y_;
    w = w_;
    h = h_;
    col = col_;
  }

  void show() {
    pushStyle();
    rectMode(CORNERS);
    fill(col);
    noStroke();
    rect(x, y, x + w, y + h);
    popStyle();
  }

  boolean intersects(Rectangle o) {
    // Works with frog smaller than obsticles, checking the obsticles
    float top = y;
    float bottom = y + h;
    float left = x;
    float right = x + w;

    float Otop = o.y;
    float Obottom = o.y + o.h;
    float Oleft = o.x;
    float Oright = o.x + o.w;

    return !(
      (top <= Obottom) ||
      (bottom >= Otop) ||
      (right <= Oleft) ||
      (left <= Oright));
  }
}
