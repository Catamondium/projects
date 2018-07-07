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
    for (PVector P : coords) {
      P.add(new PVector(x, y));
    }
  }

  void trans(PVector v) {
    trans(v.x, v.y);
  }

  void rot() {
    switch(type) {
    case 0: // between coords[2] & coords[3], but to a side
      centred_rot(I_centre());
      break;

    case 1: // NOOP
      break;

    default:
      centred_rot(coords[3]);
      break;
    }

    rotation++;
    if (rotation > 3 || rotation < 0)
      rotation = 0;
  }

  void centred_rot(PVector C) {
    PVector tmpC = C.copy();
    trans(-C.x, -C.y); // translate to (0, 0)

    for (PVector P : coords) { // rotate each point clockwise
      float tmp = P.x;
      P.x = -P.y;
      P.y = tmp;
    }

    trans(tmpC); // restore position
  }

  PVector I_centre() {
    PVector ret = new PVector();

    PVector mean = PVector.add(coords[2], coords[3]);
    mean.div(2); // Gather mean

    float c = (rotation % 3 == 0) ? +.5 : -.5;
    if (rotation % 2 == 0) {
      ret.set(mean.x, mean.y + c);
    } else {
      ret.set(mean.x + c, mean.y);
    }

    return ret;
  }

  void show(int scale) {
    fill(T_COLS[type]);
    for (int i = 0; i < 4; i++) {
      rect(coords[i].x * scale, coords[i].y * scale, scale, scale);
    }
  }
}
