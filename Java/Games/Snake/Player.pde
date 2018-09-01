class Player {
  PVector vel = new PVector();
  ArrayList<PVector> body = new ArrayList<PVector>();

  void run() {
    snake.update();
    snake.grow();
    snake.death();
    snake.show();
  }

  Player() {
    PVector head = new PVector();
    pickLocation(head);
    body.add(head);
  }

  void update() {
    PVector head = Head().copy();
    head.add(vel);

    body.remove(0);
    body.add(head);
  }

  void show() {
    fill(255);
    for (int i = 0; i < body.size(); i++) {
      PVector v = body.get(i);
      float hue = 300;
      if (body.size()-1 != 0)
        hue = map(i, 0, body.size()-1, 273, 300);
      fill(hue, 100, 100);
      rect(v.x * scl.x, v.y * scl.y, scl.x, scl.y);
    }
    fill(255);
  }

  void dir(int x_, int y_) {
    if ((x_ != -vel.x) || (y_ != -vel.y) || body.size() == 1) // disable reverse
      vel.set(x_, y_);
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
