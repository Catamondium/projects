class Tet {
  PVector[] coords = new PVector[4];
  int type;

  Tet(int type_) {
    type = type_;
    for (int i = 0; i < 4; i++) {
      coords[i] = new PVector(TETS[type_][i][0], TETS[type_][i][1]);
    }
  }

  void trans(float x, float y) {
    for (PVector v : coords) {
      v.add(new PVector(x, y));
    }
  }
  void trans(PVector v) {
    trans(v.x, v.y);
  }

  void rot() {
    switch(type) {
    case 0: // between coords[2] & coords[3], but to a side
      //PVector mean = PVector.add(coords[2], coords[3]);
      //mean.mult(.5); // Gather mean
    case 1: // NOOP
      break;
    default:
      block_Centred(coords[3]);
      break;
    }
  }

  void block_Centred(PVector C) {
    PVector tmpC = C.copy();
    trans(-C.x, -C.y);
    for (int i = 0; i < 4; i++) {
      float tmp = coords[i].x;
      coords[i].x = -coords[i].y;
      coords[i].y = tmp;
    }
    trans(tmpC);
  }

  void show(int scale) {
    fill(T_COLS[type]);
    //rectMode(CENTER);
    for (int i = 0; i < 4; i++) {
      rect(coords[i].x * scale, coords[i].y * scale, 50, 50);
    }
  }
}
