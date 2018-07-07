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
    tiles = new Tile[w * h]; 
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
    for (PVector P : t.coords) {
      if (fetch(P.x + 1, P.y) != null 
        || P.x == w 
        || P.y == h) {
        commit(t.coords, t.type, t.rotation);
        return true;
      }
    }
    return false;
  }

  Tile fetch(float x, float y) {
    //println("fetched: " + tiles[(int)x + (int)y * w]);
    return tiles[(int)x + (int)y * w];
  }

  void commit(PVector[] points, int type, int rotation) {
    for (PVector P : points) {
      tiles[(int)P.x + (int)P.y * w] = new Tile(type, rotation);
    }
  }
}
