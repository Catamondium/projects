class Frog extends Rectangle {
  int lane = 0; // !! unconstrained
  Frog(float x_) {
    super(x_, 0, grid.x - 5, grid.y - 5, #00FF00, true);
  }

  void dir(float x_, float y_) {
    x += x_ * grid.x;
    y += y_ * grid.y;
    if (y_ > 0) {
      lane--;
    } else if (y_ < 0) {
      lane++;
    }
    lane = constrain(lane, 0, lanes.length); // insure mapping to lanes[i]
  }

  void update() {
    // Stay on screen
    frog.x = constrain(x, grid.x * 0.5, width - 0.5 * grid.x);
    frog.y = constrain(y, grid.y * 0.5, height - 0.5 * grid.y); 
    action();
  }

  void action() {
    if (lanes[lane].type > SAFETY) {
      if (lanes[lane].type == CAR) {
        for (Obsticle a : lanes[lane].obsticles) {
          if (intersects(a)) {
            ResetGame();
          }
        }
      } else {
        for (Obsticle a : lanes[lane].obsticles) {
          while (intersects(a)) {
            x = x + lanes[lane].speed;
          }
        }
      }
    }
  }
}
