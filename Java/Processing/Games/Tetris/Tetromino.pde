class Tet {
  PVector[] blocks = new PVector[4];
  int type;
  int rotation = 0;
  // Collision locks
  boolean left_enable = true;
  boolean right_enable = true;

  Tet(int type_) {
    type = type_;
    for (int i = 0; i < 4; i++) {
      blocks[i] = TETS[type_].ords[i].copy();
    }
  }

  boolean update(Matrix m) { // if true, respawn player
    strain(m);
    boolean ret = m.query(clone());
    if (ret) {
      m.commit(clone());
      m.CheckRows(blocks);
    }

    if (!debug)
      trans(0, 1);
    return ret;
  }

  void show(Matrix M) {
    pushStyle();
    fill(TETS[type].col);
    for (int i = 0; i < 4; i++) {
      if (blocks[i].y >= 0) // Don't show outside bounding box
        rect(blocks[i].x * M.scale.x + M.origin.x, 
          blocks[i].y * M.scale.y + M.origin.y, 
          M.scale.x, M.scale.y);
    }
    popStyle();
  }

  Tet clone() {
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

  // Transformation & constraint methods
  void trans(float x, float y) {
    // Translate by (x, y)
    for (PVector P : blocks) {
      P.add(new PVector(x, y));
    }
  }

  void trans(float x, float y, boolean noClip) {
    // Collision checked trans overload
    if ((!noClip && y == 0)
      && ((right_enable && x == +1)
      || (left_enable && x == -1)))
      trans(x, y);
  }

  void drop(Matrix m) {
    trans(0, m.dropBy(blocks));
  }

  boolean checkbound(PVector P, int w) {
    // left/right boundary check
    if (P.x == 0) left_enable = false;
    if (P.x == w - 1) right_enable = false;

    return !(right_enable && left_enable);
  }

  boolean checkothers(PVector P, Matrix m) {
    // check for surrounding tiles
    if (P.y >= 0) { // Array safety, insure on board
      if (left_enable && m.fetch(P.x - 1, P.y).exists)
        left_enable = false; // Left-adjacent block

      if (right_enable && m.fetch(P.x + 1, P.y).exists)
        right_enable = false; // Right-adjacent block
    }

    return !(right_enable && left_enable);
  }

  void strain(Matrix m) {
    left_enable = true;
    right_enable = true;

    FloatList ySet = new FloatList();
    for (PVector P : blocks) {
      ySet.append(P.y);
    }

    if (ySet.max() >= m.h)
      trans(0, (m.h - 1)- ySet.max());

    for (PVector P : blocks) {
      if (checkbound(P, m.w) || checkothers(P, m))
        break;
    }
  }

  void rot(int dir, Matrix M) {
    switch(type) {
    case 0: // between coords[2] & coords[3], but to a side
      centred_rot(I_centre(), dir, M);
      break;

    case 1: // NOOP
      break;

    default:
      centred_rot(blocks[3], dir, M);
      break;
    }

    rotation += (dir > 0) ? +1 : -1;
    if (rotation > 3 || rotation < 0)
      rotation = 0;
  }

  void centred_rot(PVector C, int dir, Matrix M) {
    // Rotate about centre point
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

    trans(tmpC.x, tmpC.y); // restore position

    // Collision detection
    FloatList xSet = new FloatList();

    for (PVector P : blocks) {
      xSet.append(P.x);
    }

    if (xSet.min() < 0)
      trans(-xSet.min(), 0);

    if (xSet.max() > M.w - 1)
      trans((M.w - 1) - xSet.max(), 0);
  }

  PVector I_centre() {
    PVector ret = new PVector();

    PVector mean = PVector.add(blocks[2], blocks[3]);
    mean.div(2); // Gather mean

    float c = (rotation % 3 == 0) ? +.5 : -.5;
    if (rotation % 2 == 0)
      ret.set(mean.x, mean.y + c);
    else
      ret.set(mean.x + c, mean.y);

    return ret;
  }
}
