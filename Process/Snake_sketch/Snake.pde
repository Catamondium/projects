class Snake {
  PVector coord = new PVector(0, 0);
  PVector vel = new PVector(0, 0);
  int score = 0;
  ArrayList<PVector> tail = new ArrayList<PVector>();

  void run() {
    s.death();
    s.update();
    s.show();
  }

  Snake() {
    pickLocation(coord);
  }

  void update() {
    if (score > 0) {
      if ( score == tail.size() && !tail.isEmpty()) {
        tail.remove(0);
      }
      tail.add(new PVector(coord.x, coord.y));
    }

    vel.mult(scl);
    coord.add(vel);

    coord.x = constrain(coord.x, 0, width - scl);
    coord.y = constrain(coord.y, 0, height - scl);
  }

  void show() {
    fill(255);
    for (PVector v : tail) {
      rect(v.x, v.y, scl, scl);
    }
    fill(255);
    rect(coord.x, coord.y, scl, scl);
  }

  void dir(int _x, int _y) {
    vel.set(_x, _y);
  }

  boolean eat(PVector other) {
    float d = coord.dist(other);
    if (d < 1) {
      score++;
      return true;
    } else {
      return false;
    }
  }


  void death() {
    for (int i = 0; i < tail.size(); i++) {
      PVector other = tail.get(i);
      float d = coord.dist(other);
      if (d < 1) {
        // println("Starting over");
        score = 0;
        tail.clear();
      }
    }
  }
}