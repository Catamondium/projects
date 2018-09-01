Player snake;
PVector scl;
PVector food = new PVector();
PVector grid = new PVector(30, 30);

void setup() {
  size(600, 600);
  colorMode(HSB, 360, 100, 100);
  scl = new PVector(width / grid.x, height / grid.y);
  snake = new Player();
  pickLocation(food);
}

void draw() {
  frameRate(15);
  background(0);
  snake.run();
  fill(255, 255, 0, 191);
  text("Score: " + score(), 3, 20);
  fill(#00FF00);
  rect(food.x*scl.x, food.y*scl.y, scl.x, scl.y);
}

void pickLocation(PVector entity) {
  entity.set(floor(random(grid.x)), floor(random(grid.y)));
}

void lose(Boolean tailDeath) {
  String Dstr = (tailDeath) ? "You ate yourself, " : "You fell off the world, ";
  Dstr += (score()==0) ? "You scored nothing." : ("Your scored " + score());
  println("Gameover:\t" + Dstr);

  pickLocation(food);
  snake.body.clear();
  snake.vel = new PVector();
  PVector newhead = new PVector();
  pickLocation(newhead);
  snake.body.add(newhead);
}

int score() {
  return snake.body.size()-1;
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
