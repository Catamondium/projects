int SAFETY = 0;
int CAR = 1;
int LOG = 2;
class Lane extends Rectangle {
  Obsticle[] obsticles;
  int type;
  float speed;

  Lane(float y_) {
    super(5, y_, width, grid.y, #222222, false);
    type = SAFETY;
    obsticles = new Obsticle[0];
  }

  Lane(float y_, int num, float spd, float xoff, int type_) {
    super(5, y_, width, height / lanes.length, #FFFFFF, false);
    if (type_ == CAR) {
      super.col = #555555;
    } else {
      super.col = #000033;
    }
    type = type_;
    obsticles = new Obsticle[num];
    speed = spd;

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
}
