Snake s;
int scl = 20;
PVector food;
PFont f;

void setup() {
  size(600, 600);
  f = createFont("Ubuntu Mono", 16, true);
  s = new Snake();
  pickLocation();
}

void draw() {
  frameRate(10);
  background(0);
  s.run();
  fill(255, 255, 0);
  textFont(f, 20);
  text("Score: " + s.score, 0, 15);

  if (s.eat(food)) {
    pickLocation();
  }

  keyPressed();
  fill(255, 0, 0);
  rect(food.x, food.y, scl, scl);
  
//saveFrame("snakeframes-0001.png");
//noLoop();
}

void pickLocation() {
  int cols = floor(width/scl);
  int rows = floor(height/scl);
  food = new PVector(floor(random(cols)), floor(random(rows)));
  food.mult(scl);
}

void keyPressed() {
  if (keyCode == UP) {
    s.dir(0, -1);
  } else if (keyCode == DOWN) {
    s.dir(0, 1);
  } else if (keyCode == LEFT) {
    s.dir(-1, 0);
  } else if (keyCode == RIGHT) {
    s.dir(1, 0);
  } else {
    //do nothing
  }
}