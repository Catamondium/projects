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
      blocks[i] = new PVector(TETS[type_][i][0], TETS[type_][i][1]);
    }
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
        rect(blocks[i].x * M.scale.x + M.origin.x, 
          blocks[i].y * M.scale.y + M.origin.y, 
          M.scale.x, M.scale.y);
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

  // Transformation & constraint methods
  void trans(float x, float y) {
    for (PVector P : blocks) {
      P.add(new PVector(x, y));
    }
  }

  void trans(float x, float y, boolean noClip) { // Collision checked translation
    if (!noClip && y == 0) {
      switch((int)x) {
      case +1:
        if (right_enable)
          trans(x, y);
        break;

      case -1:
        if (left_enable)
          trans(x, y);
        break;
      }
    }
  }

  void drop(Matrix m) {
    trans(0, m.dropBy(blocks));
  }

  void strain(Matrix m) {
    left_enable = true;
    right_enable = true;

    for (PVector P : blocks) {
      if (P.x == 0) // Left matrix boundary
        left_enable = false;

      if (P.x == m.w - 1) // Right matrix boundary
        right_enable = false;

      if (P.y >= 0) { // Array safety, insure on board
        if (left_enable == true && m.fetch(P.x - 1, P.y).exists)
          left_enable = false; // Left-adjacent block

        if (right_enable == true && m.fetch(P.x + 1, P.y).exists)
          right_enable = false; // Right-adjacent block
      }
    }
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

    rotation += (dir > 0) ? +1 : -1;
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

    trans(tmpC.x, tmpC.y); // restore position
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
