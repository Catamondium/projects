class Tet {
  PVector[] coords = new PVector[4];
  int type;
  int rotation = 0;
  PVector origin, dimentions;

  Tet(int type_, PVector origin_, PVector dimentions_) {
    type = type_;
    origin = origin_;
    dimentions = dimentions_;
    for (int i = 0; i < 4; i++) {
      coords[i] = new PVector(TETS[type_][i][0], TETS[type_][i][1]);
    }
  }

  Tet copy() {
    Tet t = new Tet(type, origin, dimentions);
    t.rotation = rotation;
    t.type = type;
    t.coords = coords;
    return t;
  }

  boolean update(int scale, Matrix m) {
    //trans(0, 1);
    strain(scale);
    boolean ret = m.query(copy());
    if (ret)
      m.commit(copy());
    return ret;
  }

  void trans(float x, float y) {
    for (PVector P : coords) {
      P.add(new PVector(x, y));
    }
  }

  void trans(PVector v) {
    trans(v.x, v.y);
  }

  void strain(int scale) {
    boolean left = false;
    boolean right = false;
    for (PVector P : coords) {
      if (P.x < origin.x) {
        left = true;
        break;
      } else if ((P.x + 1) * scale > dimentions.x + origin.x) {
        right = true;
        break;
      }
    }

    if (left)
      trans(1, 0);
    if (right)
      trans(-1, 0);
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
      rect(coords[i].x * scale + origin.x, coords[i].y * scale + origin.y, scale, scale);
    }
  }
}
