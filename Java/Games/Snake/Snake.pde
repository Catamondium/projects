Player snake;
PVector food = new PVector();
PVector scl;
PVector GRID = new PVector(30, 30);
boolean start;

// snake colouration
float huetail = 273; // purple
float huehead = 300; // pink

color foodcol = #00FF00;

void setup() {
  size(600, 600);
  colorMode(HSB, 360, 100, 100);
  scl = new PVector(width / GRID.x, height / GRID.y);
  snake = new Player();
  pickLocation(food);
}

void draw() {
  frameRate(15);
    background(0);

    snake.update();
    snake.show();

    fill(foodcol);
    rect(food.x*scl.x, food.y*scl.y, scl.x, scl.y);

    fill(60, 60, 100, 191);
    text("Score: " + score(), 3, 20);
}

void pickLocation(PVector entity) {
  entity.set(floor(random(GRID.x)), floor(random(GRID.y)));
}

void lose(Boolean tailDeath) {
  String Dstr = (tailDeath) ?
    "You ate yourself, " : "You fell off the world, ";

  Dstr += (score() == 0) ?
    "scoring nothing." : ("scoring " + score());

  println(Dstr);

  pickLocation(food);
  snake = new Player();
}

int score() {
  return snake.body.size()-4;
}

void keyPressed() {
  switch(key) {
  case 'w':
    snake.dir(0, -1);
    break;

  case 's':
    snake.dir(0, 1);
    break;

  case 'a':
    snake.dir(-1, 0);
    break;

  case 'd':
    snake.dir(1, 0);
    break;

  case 'e':
    saveFrame("output.png");
    break;
  }
}
