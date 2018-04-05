class Frog extends Rectangle {
  int lane = 0; // !! unconstrained
  Frog(float x_) {
    super(x_, 0, grid.x - 5, grid.y - 5, #00FF00);
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
    frog.x = constrain(x, grid.x * 0.5, width - 0.5 * grid.x);
    frog.y = constrain(y, grid.y * 0.5, height - 0.5 * grid.y);
  }
}
