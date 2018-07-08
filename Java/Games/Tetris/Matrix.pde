class Tile { //<>//
  boolean exists;
  int type, rotation;

  void insert(int type_, int rotation_) {
    type = type_;
    rotation = rotation_;
    exists = true;
  }
  void insert(Tile t) {
    type = t.type;
    rotation = t.rotation;
    exists = true;
  }
}

class Matrix {
  Tile[] tiles;
  int w, h;
  PVector origin, dimentions;

  Matrix(int w_, int h_, PVector origin_, PVector dimentions_) {
    origin = origin_;
    w = w_;
    h = h_;
    dimentions = dimentions_;
    tiles = new Tile[w * h];
    for (int i = 0; i < tiles.length; i++) {
      tiles[i] = new Tile();
    }
  }

  void show(PVector dimentions) {
    pushStyle();
    noFill();

    stroke(#FF0000);
    rect(origin.x, origin.y, dimentions.x, dimentions.y);
    PVector scale = get_scale();

    stroke(0);
    for (int x = 0; x < w; x++) {
      for (int y = 0; y < h; y++) {
        if (fetch(x, y).exists) {
          Tile t = fetch(x, y);
          fill(T_COLS[t.type]);
          rect((x * scale.x) + origin.x, (y * scale.y) + origin.y, scale.x, scale.y);
        }
      }
    }
    popStyle();
  }

  PVector get_scale() {
    return new PVector(dimentions.x / w, dimentions.y / h);
  }

  boolean query(Tet t) {
    boolean ret = false;

    for (PVector P : t.blocks) {      
      if (query_test(P)) {
        ret = true;
        break;
      }
    }
    return ret;
  }

  Tile fetch(float x, float y) {
    return tiles[ord(x, y)];
  }

  boolean query_test(PVector B) {
    int under_me = ord(B.x, B.y + 1);

    if (under_me < 0) // cover for starting position when translated to (x, -2)
      return false;

    else if (under_me >= (w * h)) // Short circuit to avoid OutOfBounds error
      return true;

    else if (tiles[under_me].exists || B.y == h)
      return true;

    return false;
  }

  int ord(float x, float y) {
    return floor((x + (y * w)));
  }

  void CheckRows(PVector[] blocks) {
    FloatList toRemove = new FloatList();
    for (PVector B : blocks) {
      if (CheckRow(B.y))
        toRemove.append(B.y);
    }

    if (toRemove.size() != 0) {
      ClearRows(toRemove);
      //MovRows(toRemove);
    }
  }

  boolean CheckRow(float y) {
    for (int x = 0; x < w; x++) {
      if (!fetch(x, y).exists)
        return false;
    }
    return true;
  }

  void ClearRows(FloatList r) {
    for (int y = 0; y < r.size(); y++ ) {
      for (int x = 0; x < w; x++) {
        tiles[ord(x, r.get(y))].exists = false;
      }
    }
  }

  //void MovRows(FloatList y) { // not yet functional
  //  float y_min = y.max();
  //  for (float y_o = y_min; y_o >= 0; y_o--) {
  //    for (int x = w; x >= 0; x--) {
  //      if (y_o == h) {
  //        tiles[ord(x, y_o)].insert(tiles[ord(x, y_o + 1)]);
  //        tiles[ord(x, y_o)].exists = false;
  //      }
  //    }
  //  }
  //}

  void commit(Tet t) {
    for (PVector P : t.blocks) {
      tiles[ord(P.x, P.y)].insert(t.type, t.rotation);
    }
  }
}
