class Frog extends Rectangle {
  int lane = 0;
  Frog(float x_) {
    super(x_, height - grid.y, grid.x, grid.y, #00FF00);
  }

  void dir(float x_, float y_) {
    x += x_ * grid.x;
    y += y_ * grid.y;
  }

  void update() {
    frog.x = constrain(x, 0, width - w); // Slide off 'logs'
  }
}
