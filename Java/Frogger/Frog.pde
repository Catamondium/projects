class Frog extends Rectangle {
  int lane = 0; // !! unconstrained
  Frog(float x_) {
    super(x_, 1 + height - grid.y, grid.x - 2, grid.y - 2, #00FF00);
  }

  void dir(float x_, float y_) {
    x += x_ * grid.x;
    y += y_ * grid.y;
    if (y_ > 0) {
      lane--;
    } else if (y_ < 0) {
      lane++;
    }
    lane = constrain(lane, 0, floor(width / grid.x) - 1); // insure mapping to lanes[i]
  }

  void update() {
    // Stay on screen
    frog.x = constrain(x, 1, width - w - 1);
    frog.y = constrain(y, 1, height - h - 1);
  }
}
