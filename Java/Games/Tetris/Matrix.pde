class Tile { //<>//
  boolean exists;
  int type, rotation;

  void insert(int type_, int rotation_) {
    type = type_;
    rotation = rotation_;
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

  void show(int scale) {
    pushStyle();
    noFill();

    stroke(#FF0000);
    rect(origin.x, origin.y, w * scale, h * scale);

    stroke(0);
    for (int x = 0; x < w; x++) {
      for (int y = 0; y < h; y++) {
        if (fetch(x, y).exists) {
          Tile t = fetch(x, y);
          fill(T_COLS[t.type]);
          rect((x * scale) + origin.x, (y * scale) + origin.y, scale, scale);
        }
      }
    }
    popStyle();
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
    if (ord(B.x, B.y + 1) >= (w * h)) { // Short circuit to avoid OutOfBounds error
      return true;
    } else if (fetch(B.x, B.y + 1).exists || B.y == h) {
      return true;
    }
    return false;
  }

  int ord(float x, float y) {
    return floor((x + (y * w)));
  }

  void commit(Tet t) {
    for (PVector P : t.blocks) {
      tiles[ord(P.x, P.y)].insert(t.type, t.rotation);
    }
  }
}
