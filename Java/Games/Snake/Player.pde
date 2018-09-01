class Player {
  PVector vel = new PVector();
  ArrayList<PVector> body = new ArrayList<PVector>();

  Player() {
    start = true;
    int x = floor(random(grid.x/2));
    int y = floor(random(grid.y));
    for (int i = 0; i < 4; i++) {
      PVector append = new PVector(x+i, y);
      body.add(append);
    }
  }

  void update() {
    if (vel.x != 0 || vel.y !=0) {
      PVector head = Head().copy();
      head.add(vel);
      body.remove(0);
      body.add(head);
    }
    death();
    grow();
  }

  void show() {
    fill(255);
    for (int i = 0; i < body.size(); i++) {
      PVector v = body.get(i);
      float hue = map(i, 0, body.size()-1, huetail, huehead);
      fill(hue, 100, 100);
      rect(v.x * scl.x, v.y * scl.y, scl.x, scl.y);
    }
    fill(255);
  }

  void dir(int x_, int y_) {
    if (start && x_==-1)
      return;
    else if ((x_ != -vel.x) || (y_ != -vel.y)) { // disable reverse
      vel.set(x_, y_);
      start = false;
    }
  }

  void grow() {
    PVector head = Head();
    if (head.x == food.x && head.y == food.y) {
      PVector append = PVector.add(Head(), vel);
      body.add(append);
      pickLocation(food);
    }
  }

  void death() {
    PVector head = Head();
    if (edges()) { // boundary collision
      lose(false);
      return;
    }

    for (int i = 0; i < body.size()-1; i++) {
      PVector other = body.get(i);
      // tail collision
      if ((head.x == other.x) && (head.y == other.y)) {
        lose(true);
        return;
      }
    }
  }

  PVector Head() {
    return body.get(body.size()-1);
  }

  Boolean edges() { // collides with boundary
    PVector head = Head();
    if (
      (head.x > grid.x) ||
      (head.x < 0) ||
      (head.y > grid.y) ||
      (head.y < 0)) {
      return true;
    }
    return false;
  }
}
