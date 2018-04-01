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
  int occupancy = 0;
  PVector[] points;
  boolean divided = false;

  // Children trees
  QuadTree NorthEast, NorthWest, SouthEast, SouthWest;

  QuadTree(Rectangle boundary_, int n) {
    boundary = boundary_;
    points = new PVector[n];
  }

  void subdivide() {
    float north = boundary.y - boundary.h / 2;
    float east = boundary.x + boundary.w / 2;
    float south = boundary.y + boundary.h / 2;
    float west = boundary.x - boundary.w / 2;
    float new_width = boundary.w / 2;
    float new_height = boundary.h / 2;

    Rectangle nw = new Rectangle(west, north, new_width, new_height);
    NorthWest = new QuadTree(nw, points.length);

    Rectangle ne = new Rectangle(east, north, new_width, new_height);
    NorthEast = new QuadTree(ne, points.length);

    Rectangle sw = new Rectangle(west, south, new_width, new_height);
    SouthWest = new QuadTree(sw, points.length);

    Rectangle se = new Rectangle(east, south, new_width, new_height);
    SouthEast = new QuadTree(se, points.length);

    divided = true;
  }

  boolean insert(PVector a) {
    if (!boundary.contains(a)) {
      return false;
    }

    if (occupancy < points.length) {
      points[occupancy] = new PVector(a.x, a.y);
      occupancy++;
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
      for (int i = 0; i < occupancy; i++) {
        if (range.contains(points[i])) {
          found.add(points[i]);
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

  void debug() {
    pushStyle();
    rectMode(CENTER);
    stroke(255);
    strokeWeight(1);
    noFill();
    rect(boundary.x, boundary.y, boundary.w * 2, boundary.h * 2);

    for (int i = 0; i < occupancy; i++) {
      stroke(255);
      strokeWeight(3);
      point(points[i].x, points[i].y);
    }

    if (divided) {
      NorthWest.debug();
      NorthEast.debug();
      SouthEast.debug();
      SouthWest.debug();
    }
    popStyle();
  }
}
