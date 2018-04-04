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
    stroke(#FF00FF);
    strokeWeight(2);
    rect(x, y, x + w, y + h);
    popStyle();
  }

  boolean intersects(Rectangle o) {
    /* TODO: side intersection
    Checking if 'o' contains my center.
    !!! NOT functional for objects with width greater than 2 * grid !!!
    **/
    PVector center = new PVector(x + 0.5 * w, y + 0.5 * h);

    float Otop = o.y;
    float Obottom = o.y + o.h;
    float Oleft = o.x;
    float Oright = o.x + o.w;

    return (((center.x >= Oleft) &&
      (center.x <= Oright)) &&
      ((center.y >= Otop) &&
      (center.y <= Obottom)));
  }
}
