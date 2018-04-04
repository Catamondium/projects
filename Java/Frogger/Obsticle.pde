class Obstical extends Rectangle {
  float speed;
  Obstical(float x_, float y_, float w_, float speed_, color col_) {
    super(x_, y_, w_, grid.y, col_);
    speed = speed_;
  }

  void update() {
    // lane checks intersection
    edges();
  }

  void edges() {
    if ((speed > 0) && (x > width + grid.x)) {
      x = - w - grid.x;
    } else if ((speed < 0) && (x < -w - grid.x)) {
      x = width + w + grid.x;
    }
  }
}
