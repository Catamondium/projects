class Tet {
  PVector[] coords = new PVector[4];
  int type;
  int rotation = 0;

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
      PVector centre = I_centre();
      block_Centred(centre);
      break;

    case 1: // NOOP
      break;

    default:
      block_Centred(coords[3]);
      break;
    }
    rotation++;
    if (rotation > 3 || rotation < 0)
      rotation = 0;
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

  PVector I_centre() {
    PVector ret = new PVector();

    PVector mean = PVector.add(coords[2], coords[3]);
    mean.mult(.5); // Gather mean

    switch(rotation) {
    case 0:
      ret.set(mean.x, mean.y + .5);
      break;

    case 1:
      ret.set(mean.x - .5, mean.y);
      break;

    case 2:
      ret.set(mean.x, mean.y - .5);
      break;

    case 3:
      ret.set(mean.x + .5, mean.y);
      break;
    }
    return ret;
  }

  void show(int scale) {
    fill(T_COLS[type]);
    for (int i = 0; i < 4; i++) {
      rect(coords[i].x * scale, coords[i].y * scale, 50, 50);
    }
  }
}
