class Frog extends Rectangle {
  int count = 0; // !! unconstrained
  Frog() {
    super(0, 0, grid.x - 5, grid.y - 5, #00FF00, true);
  }

  void dir(float x_, float y_) {
    x += x_ * grid.x;
    y += y_ * grid.y;
    if (y_ > 0) {
      count--;
    } else if (y_ < 0) {
      count++;
    }
    count = constrain(count, 0, lanes.length); // insure mapping to lanes[i]
  }

  void update() {
    // Stay on screen
    frog.x = constrain(x, grid.x * 0.5, width - 0.5 * grid.x);
    frog.y = constrain(y, grid.y * 0.5, height - 0.5 * grid.y);
  }
}
