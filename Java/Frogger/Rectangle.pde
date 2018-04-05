class Rectangle {
  float x, y, w, h;
  color col = #000000;

  Rectangle(float x_, float y_, // Constructed relative to center
    float w_, float h_, color col_) {
    x = ((x_ + 1) * grid.x) - (0.5 * grid.x);
    y = height - ((y_ + 1) * grid.y) + (0.5 * grid.y);
    w = w_;
    h = h_;
    col = col_;
  }

  void show() {
    pushStyle();
    rectMode(CENTER);
    fill(col);
    noStroke();
    rect(x, y, w, h);
    popStyle();
  }

  boolean intersects(Rectangle o) {
    // Works with frog smaller than obsticles, checking the obsticles
    float top = y - 0.5 * h;
    float bottom = y + 0.5 * h;
    float left = x - 0.5 * w;
    float right = x  + 0.5 * w;

    float Otop = o.y - 0.5 *  o.h;
    float Obottom = o.y + 0.5 * o.h;
    float Oleft = o.x - 0.5 * o.w;
    float Oright = o.x + 0.5 * o.w;

    return !(
      (top <= Obottom) ||
      (bottom >= Otop) ||
      (right <= Oleft) ||
      (left <= Oright));
  }
}
