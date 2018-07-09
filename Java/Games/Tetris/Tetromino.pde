class Tet {
  PVector[] blocks = new PVector[4];
  int type;
  int rotation = 0;

  Tet(int type_) {
    type = type_;
    for (int i = 0; i < 4; i++) {
      blocks[i] = new PVector(TETS[type_][i][0], TETS[type_][i][1]);
    }
    //trans(0, 3);
  }

  boolean update(Matrix m) { // if true, replace player
    strain(m);
    boolean ret = m.query(copy());
    if (ret) {
      m.commit(copy());
      m.CheckRows(blocks);
    }
    //trans(0, 1);
    return ret;
  }

  void show(Matrix M) {
    pushStyle();
    fill(T_COLS[type]);
    for (int i = 0; i < 4; i++) {
      if (blocks[i].y >= 0) // Don't show outside bounding box
        rect(blocks[i].x * M.calcScale().x + M.origin.x, 
          blocks[i].y * M.calcScale().y + M.origin.y, 
          M.calcScale().x, M.calcScale().y);
    }
    popStyle();
  }

  Tet copy() {
    Tet t = new Tet(type);
    t.rotation = rotation;
    t.type = type;
    t.blocks = blocks;
    return t;
  }

  boolean above_board() {
    for (PVector B : blocks) {
      if (B.y < 0)
        return true;
    }
    return false;
  }

  // Translation & constraint methods
  void trans(float x, float y) {
    for (PVector P : blocks) {
      P.add(new PVector(x, y));
    }
  }

  void trans(PVector v) {
    trans(v.x, v.y);
  }

  void drop(Matrix m) { // Doesn't function above the Matrix, as expected
    trans(0, m.dropBy(blocks));
  }

  void strain(Matrix m) {
    boolean left = false;
    boolean right = false;

    for (PVector P : blocks) {
      if (P.x < 0) { //|| m.Bfetch((P.x - 1), P.y)) {
        left = true;
        break;
      } else if (P.x + 1 > m.w) {// || m.Bfetch((P.x + 1) ,P.y)) {
        right = true;
        break;
      }
    }

    if (left)
      trans(1, 0);

    if (right)
      trans(-1, 0);
  }

  void rot(int dir) {
    switch(type) {
    case 0: // between coords[2] & coords[3], but to a side
      centred_rot(I_centre(), dir);
      break;

    case 1: // NOOP
      break;

    default:
      centred_rot(blocks[3], dir);
      break;
    }

    rotation++;
    if (rotation > 3 || rotation < 0)
      rotation = 0;
  }

  void centred_rot(PVector C, int dir) {
    PVector tmpC = C.copy();
    trans(-C.x, -C.y); // translate to (0, 0)

    for (PVector P : blocks) { // rotate each point clockwise
      if (dir > 0) {
        float tmp = P.x;
        P.x = -P.y;
        P.y = tmp;
      } else {
        float tmp = P.y;
        P.y = -P.x;
        P.x = tmp;
      }
    }

    trans(tmpC); // restore position
  }

  PVector I_centre() {
    PVector ret = new PVector();

    PVector mean = PVector.add(blocks[2], blocks[3]);
    mean.div(2); // Gather mean

    float c = (rotation % 3 == 0) ? +.5 : -.5;
    if (rotation % 2 == 0) {
      ret.set(mean.x, mean.y + c);
    } else {
      ret.set(mean.x + c, mean.y);
    }

    return ret;
  }
}
