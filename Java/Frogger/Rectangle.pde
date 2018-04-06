class Rectangle {
  float x, y, w, h;
  color col = #000000;
  boolean centered;

  Rectangle(float x_, float y_, // Constructed relative to center
    float w_, float h_, color col_, boolean centered_) {
    if (centered_) {
      x = ((x_ + 1) * grid.x) - (0.5 * grid.x);
    } else {
      x = ((x_ + 1) * grid.x) - grid.x;
    }
    y = height - ((y_ + 1) * grid.y) + (0.5 * grid.y);
    w = w_;
    h = h_;
    col = col_;
    centered = centered_;
  }

  void show() {
    pushStyle();
    rectMode(CENTER);
    fill(col);
    noStroke();
    rect(x, y, w, h);
    popStyle();
  }
  int count = 0;
  boolean intersects(Rectangle o) { // Never true?
    // Works with frog smaller than obsticles
    float top, bottom, left, right;
    float Otop, Obottom, Oleft, Oright;

    top = y - (0.5 * h);
    bottom = y + (0.5 * h);
    left = x - (0.5 * w);
    right = x  + (0.5 * w);

    Otop = o.y - (0.5 *  o.h);
    Obottom = o.y + (0.5 * o.h);
    Oleft = o.x - (0.5 * o.w);
    Oright = o.x + (0.5 * o.w);

    boolean ret = !(
      (top >= Obottom) ||
      (bottom <= Otop) ||
      (right <= Oleft) ||
      (left >= Oright));

    if (ret == true) { // Debug probe
      println("True");
    }
    return ret;
  }
}
