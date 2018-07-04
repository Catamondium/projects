class Lane extends Rectangle {
  Obsticle[] obsticles;
  int type;

  Lane(float y_) { // Make safety at y
    super(5, y_, width, grid.y, LANE_SAFETY, false);
    type = SAFETY;
    obsticles = new Obsticle[0];
  }

  Lane(float y_, int num, float spd, float xoff, int type_) { // Make hostile lanes
    super(5, y_, width, height / lanes.length, #FFFFFF, false);
    type = type_;
    obsticles = new Obsticle[num];
    super.col = (type_ == CAR) ? LANE_CAR : LANE_LOG; // Set background from type
    color colO = (type_ == CAR) ? COL_CAR : COL_LOG; // Set type colour

    // Initalise obsticles
    for (int i = 0; i < obsticles.length; i++) {
      obsticles[i] = new Obsticle(grid.x * i + xoff, y_, 2 * grid.x, spd, colO);
    }
  }

  void update() {
    for (Obsticle a : obsticles) {
      a.update();
    }
  }

  void display() {
    show();
    for (Obsticle a : obsticles) {
      a.show();
    }
  }

  void run() {
    update();
    display();
  }

  void check(Frog frog) {
    if (type == CAR) {
      for (Obsticle o : obsticles) {
        if (o.intersects(frog)) {
          GameOver();
          break;
        }
      }
    } else if (type == LOG) {
      boolean ok = false;
      for (Obsticle o : obsticles) {
        if (o.intersects(frog)) {
          ok = true;
          frog.attach(o);
          break;
        }
      }
      if (!ok) {
        GameOver();
      }
    }
  }
}
