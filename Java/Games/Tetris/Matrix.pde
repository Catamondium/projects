class Tile { //<>//
  boolean exists = false;
  int type, rotation;

  void insert(int type_, int rotation_) {
    type = type_;
    rotation = rotation_;
    exists = true;
  }
  void insert(Tile t) {
    if (t.exists) {
      type = t.type;
      rotation = t.rotation;
      exists = true;
    }
  }
}

class Matrix {
  Tile[] tiles;
  int w, h;
  PVector origin, dimentions, scale;

  Matrix(int w_, int h_, PVector origin_, PVector dimentions_) {
    origin = origin_;

    w = w_;
    h = h_;

    dimentions = dimentions_;
    scale = new PVector(dimentions_.x / w_, dimentions_.y / h_);

    tiles = new Tile[w * h];
    for (int i = 0; i < tiles.length; i++) {
      tiles[i] = new Tile(); // Insure !=null elements for .exists check
    }
  }

  void show(PVector dimentions) {
    pushStyle();
    noFill();

    stroke(#FF0000);
    rect(origin.x, origin.y, dimentions.x, dimentions.y);

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

  PVector calcScale() {
    return new PVector(dimentions.x / w, dimentions.y / h);
  }

  // Querying methods
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

  void commit(Tet t) {
    for (PVector P : t.blocks) {
      tiles[ord(P.x, P.y)].insert(t.type, t.rotation);
    }
  }

  Tile fetch(float x, float y) {
    return tiles[ord(x, y)];
  }

  int ord(float x, float y) {
    return floor((x + (y * w)));
  }

  // Row/column methods

  float dropBy(PVector[] blocks) {
    FloatList lengths = new FloatList();
    for (PVector B : blocks) {
      for (int y = h; (y > 0) && (y > B.y); y--) {
        if (!tiles[ord(B.x, y - 1)].exists) {
          lengths.append(y - B.y);
          break;
        }
      }
    }
    return lengths.min() - 1;
  }

  void CheckRows(PVector[] blocks) {
    FloatList toRemove = new FloatList();

    for (PVector B : blocks) {
      if (!toRemove.hasValue(B.y) && CheckRow(B.y))
        toRemove.append(B.y);
    }

    if (toRemove.size() > 0) {
      ClearRows(toRemove);
      MovRows(toRemove);
    }

    win(toRemove.size());
  }

  boolean CheckRow(float y) {
    for (int x = 0; x < w; x++) {
      if (!fetch(x, y).exists)
        return false;
    }
    return true;
  }

  void ClearRows(FloatList y) {
    for (int i = ord(0, y.min()); i < ord(w, y.max()); i++ ) {
      tiles[i].exists = false;
    }
  }

  void MovRows(FloatList y) {
    int start = ord(w - 1, y.min() - 1); // offset x for 0 index
    int mov_by = y.size() * w;

    println(y.min(), mov_by);

    for (int i = start; i >= 0; i--) {
      tiles[i + mov_by].insert(tiles[i]);
      tiles[i].exists = false;
    }
  }
}
