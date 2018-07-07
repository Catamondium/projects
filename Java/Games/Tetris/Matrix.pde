class Tile {
  int type, rotation;

  Tile(int type_, int rotation_) {
    type = type_;
    rotation = rotation_;
  }
}

class Matrix {
  Tile[] tiles; // NEVER returns null!
  boolean[] checks;
  int w, h;
  PVector origin, dimentions;

  Matrix(int w, int h, PVector origin_, PVector dimentions_) {
    tiles = new Tile[w * h];
    checks = new boolean[w * h];
    origin = origin_;
    dimentions = dimentions_;
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

  boolean query(Tet t) {
    boolean ret = false;
    for (PVector P : t.coords) {
      if (fetch(P.x + 1, P.y) != null 
        || P.x == w 
        || P.y == h) {
        ret = true;
        break;
      }
    }
    println(ret);
    return ret;
  }

  Tile fetch(float x, float y) {
    if (checks[ord(x, y)]) {
      return tiles[ord(x, y)];
    } else { 
      return null;
    }
  }

  int ord(float x, float y) {
    return (int)(x + y * w);
  }

  void commit(Tet t) {
    for (PVector P : t.coords) {
      tiles[ord(P.x, P.y)] = new Tile(t.type, t.rotation);
      checks[ord(P.x, P.y)] = true;
    }
  }
}
