class Rectangle {
  float x, y, w, h;
  color col;
  boolean centered;

  Rectangle(float x_, float y_, // Constructed relative to center
    float w_, float h_, color col_, boolean centered_) {
    x = (x_ + 1) * grid.x;
    
    x -= (centered_) ? 0.5 * grid.x : grid.x; // Correct between lanes vs characters

    y = height - (y_ + 0.5) * grid.y;
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
