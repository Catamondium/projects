int SAFETY = 0;
int CAR = 1;
int LOG = 2;
class Lane extends Rectangle {
  Obsticle[] obsticles;
  int type;

  Lane(float y_) { // Make safety at y
    super(5, y_, width, grid.y, #222222, false);
    type = SAFETY;
    obsticles = new Obsticle[0];
  }

  Lane(float y_, int num, float spd, float xoff, int type_) { // Make hostile lanes
    super(5, y_, width, height / lanes.length, #FFFFFF, false);
    type = type_;
    obsticles = new Obsticle[num]; // Initalise obsticles array
    if (type_ == CAR) { // Override color depending on type
      super.col = #555555;
    } else {
      super.col = #000033;
    }

    // Initalise obsticles
    for (int i = 0; i < obsticles.length; i++) {
      color colO = #FFFFFF;
      if (type_ == CAR) {
        colO =  #FF0000;
      }
      if (type_ == LOG) {
        colO = #654321;
      }
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
        }
      }
    } else if (type == LOG) {
      boolean ok = false;
      for (Obsticle o : obsticles) {
        if (o.intersects(frog)) {
          ok = true;
          frog.attach(o);
        }
      }
      if (!ok) {
        GameOver();
      }
    }
  }
}
