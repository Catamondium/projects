class Frog extends Rectangle {
  Obsticle attached;
  Frog() {
    super(0, 0, grid.x - 4, grid.y - 4, colours[FROG].obj, true);
  }

  void update() {
    // Constrain position
    player.x = constrain(x, grid.x * 0.5, width - 0.5 * grid.x);
    player.y = constrain(y, grid.y * 0.5, height - 0.5 * grid.y);

    if (attached != null) { // If on log, follow log
      x += attached.speed;
    }

    if (myLane() == lanes.length - 1) { // Check win
      GameWon();
    }
  }

  void dir(float x_, float y_) { // Move by multiples of grid
    x += x_ * grid.x;
    y += y_ * grid.y;
    attach(null);
  }

  void attach(Obsticle o) {
    attached = o;
  }

  int myLane() {
    int ret = (lanes.length - int(player.y / grid.y) - 1);
    ret = constrain(ret, 0, lanes.length-1);
    return ret;
  }
}
