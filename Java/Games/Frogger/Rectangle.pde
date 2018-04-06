class Rectangle {
  float x, y, w, h;
  color col = #000000;
  boolean centered;

  Rectangle(float x_, float y_, // Constructed relative to center
    float w_, float h_, color col_, boolean centered_) {
    x = ((x_ + 1) * grid.x);

    if (centered_) { // Correct for lanes
      x -= (0.5 * grid.x);
    } else {
      x -= grid.x;
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

  boolean intersects(Rectangle o) {
    float left = x - (0.5 * w);
    float right = x  + (0.5 * w);

    float Oleft = o.x - (0.5 * o.w);
    float Oright = o.x + (0.5 * o.w);

    return !( // Called from lanes[myLane()], no need to check y.
      (right <= Oleft) ||
      (left >= Oright));
  }
}
