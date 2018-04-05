class Frog extends Rectangle {
  Obsticle attached;
  Frog() {
    super(0, 0, grid.x - 4, grid.y - 4, #00FF00, true);
  }

  void dir(float x_, float y_) { // Move by multiples of grid
    x += x_ * grid.x;
    y += y_ * grid.y;
    attach(null);
  }

  void attach(Obsticle o) {
    attached = o;
  }

  void update() {
    if (attached != null) {
      x += attached.speed;
    }
    if (myLane() == lanes.length) {
      GameOver();
    }
    // Constrain position
    player.x = constrain(x, grid.x * 0.5, width - 0.5 * grid.x);
    player.y = constrain(y, grid.y * 0.5, height - 0.5 * grid.y);
  }

  int myLane() {
    return (lanes.length - int(player.y / grid.y) - 1);
  }
}
