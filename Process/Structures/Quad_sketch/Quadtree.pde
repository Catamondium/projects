class Rectangle {
  float x, y, w, h;
  Rectangle(float x_, float y_, float w_, float h_) {
    x = x_;
    y = y_;
    w = w_;
    h = h_;
  }

  boolean contains(PVector a) {
    return ((a.x >= x - w) &&
      (a.x <= x + w) &&
      (a.y >= y - h) &&
      (a.y <= y + h));
  }

  boolean intersects(Rectangle range) {
    return !((range.x - range.w > x + w) ||
      (range.x + range.w < x - w) ||
      (range.y - range.h > y + h) ||
      (range.y + range.h < y - h));
  }
}

class QuadTree {
  Rectangle boundary;
  int capacity;
  ArrayList<PVector> points;
  boolean divided = false;

  // Children trees
  QuadTree NorthEast, NorthWest, SouthEast, SouthWest;

  QuadTree(Rectangle boundary_, int n) {
    boundary = boundary_;
    capacity = n;
    points = new ArrayList<PVector>(n);
  }

  void subdivide() {
    float north = boundary.y - boundary.h / 2;
    float east = boundary.x + boundary.w / 2;
    float south = boundary.y + boundary.h / 2;
    float west = boundary.x - boundary.w / 2;
    float new_width = boundary.w / 2;
    float new_height = boundary.h / 2;

    Rectangle nw = new Rectangle(west, north, new_width, new_height);
    NorthWest = new QuadTree(nw, capacity);

    Rectangle ne = new Rectangle(east, north, new_width, new_height);
    NorthEast = new QuadTree(ne, capacity);

    Rectangle sw = new Rectangle(west, south, new_width, new_height);
    SouthWest = new QuadTree(sw, capacity);

    Rectangle se = new Rectangle(east, south, new_width, new_height);
    SouthEast = new QuadTree(se, capacity);

    divided = true;
  }

  boolean insert(PVector a) {
    if (!boundary.contains(a)) {
      return false;
    }

    if (points.size() < capacity) {
      points.add(a);
      return true;
    } else {
      if (!divided) {
        subdivide();
      }
      if (NorthEast.insert(a) ||
        NorthWest.insert(a) ||
        SouthEast.insert(a) ||
        SouthWest.insert(a)) {
        return true;
      } else {
        return false;
      }
    }
  }

  ArrayList query(Rectangle range, ArrayList<PVector> found) {
    if (!boundary.intersects(range)) {
      return found;
    } else {
      for (PVector p : points) {
        if (range.contains(p)) {
          found.add(p);
        }
      }
      if (divided) {
        NorthWest.query(range, found);
        NorthEast.query(range, found); 
        SouthWest.query(range, found); 
        SouthEast.query(range, found);
      }
      return found;
    }
  }

  void show() {
    rectMode(CENTER);
    stroke(255);
    strokeWeight(1);
    noFill();
    rect(boundary.x, boundary.y, boundary.w * 2, boundary.h * 2);

    for ( PVector p : points) {
      stroke(255, 0, 0);
      strokeWeight(3);
      point(p.x, p.y);
    }

    if (divided) {
      NorthWest.show();
      NorthEast.show();
      SouthEast.show();
      SouthWest.show();
    }
  }
}
