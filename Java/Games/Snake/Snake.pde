Player s;
final int scl = 20;
PVector food = new PVector();
//PFont f;

void setup() {
  size(600, 600);
  //f = createFont("Roboto Regular", 20, false);
  s = new Player();
  pickLocation(food);
}

void draw() {
  frameRate(10);
  background(0);
  s.run();
  fill(255, 255, 0, 191);
  //textFont(f, 20);
  text("Score: " + s.score, 3, 20);

  if (s.eat(food)) {
    pickLocation(food);
  }

  keyPressed();
  fill(255, 0, 0);
  rect(food.x, food.y, scl, scl);
}

void pickLocation(PVector entity) {
  int cols = floor(width/scl);
  int rows = floor(height/scl);
  entity.set(floor(random(cols)), floor(random(rows)));
  entity.mult(scl);
}

void keyPressed() {
  switch(keyCode) {
  case UP:
    s.dir(0, -1);
    break;
  case DOWN:
    s.dir(0, 1);
    break;
  case LEFT:
    s.dir(-1, 0);
    break;
  case RIGHT:
    s.dir(1, 0);
    break;
  }
}

void mousePressed() {
  saveFrame("output.png");
}
