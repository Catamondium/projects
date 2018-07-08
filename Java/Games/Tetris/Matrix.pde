class Tile {
  int type, rotation;

  Tile(int type_, int rotation_) {
    type = type_;
    rotation = rotation_;
  }
}

class Matrix {
  Tile[] tiles;
  int w, h;
  PVector origin, dimentions;

  Matrix(int w, int h, PVector origin_, PVector dimentions_) {
    origin = origin_;
    dimentions = dimentions_;
    tiles = new Tile[w * h];
  }

  void show(int scale) {
    noFill();
    stroke(255);
    rect(origin.x, origin.y, dimentions.x, dimentions.y);
    for (int x = 0; x < w; x++) {
      for (int y = 0; y < h; y++) {
        if (fetch(x, y) != null) {
          Tile t = fetch(x, y);
          fill(T_COLS[t.type]);
          rect(x * scale + origin.x, y * scale + origin.y, scale, scale);
        }
      }
    }
  }

  boolean query(Tet t) { // Error causer
    boolean ret = false;
    for (PVector P : t.blocks) {
      if ((tiles[ord(P.x, P.y)] == null) // always TRUE, regardless of == or != or direct access, .equals() results in nullptr exception, causing issues
        || (P.y >= h)) { /// Always false
        Boolean booltest_1 = (P.y == h ) ? true : false; // probe start
        Boolean booltest_2 = (fetch(P.x, P.y) != null) ? true : false;
        print("height: " + booltest_1 + "\tpresence: " + booltest_2 + "\n"); // probe end
        ret = true;
        break;
      }
    }
    return ret;
  }

  Tile fetch(float x, float y) {
    return tiles[ord(x, y)];
  }

  int ord(float x, float y) {
    return (int)(x + y * w);
  }

  void commit(Tet t) {
    for (PVector P : t.blocks) {
      tiles[ord(P.x, P.y)] = new Tile(t.type, t.rotation);
    }
  }
}
