Frog frog;
Obstical obj;
PVector grid;

void ResetGame() {
  frog = new Frog(1 + 0.5 * (width - grid.x));
}
void setup() {
  size(550, 550);
  grid = new PVector(50, 50); // 11 lanes, 11 tiles long
  ResetGame();
  obj = new Obstical(0.5 * (width - grid.x), height + (2 * -grid.y), grid.x * 2, -2, #FF0000);
}

void draw() {
  background(0);
  obj.update();
  frog.update();
  obj.show();
  frog.show();
  //debug();
}

void keyPressed() {
  switch(key) {
  case 'w':
    frog.dir(0, -1);
    break;
  case 's':
    frog.dir(0, 1);
    break;
  case 'a':
    frog.dir(-1, 0);
    break;
  case 'd':
    frog.dir(1, 0);
    break;
  }
}

// Debugging
void debug() {
  stroke(255, 255, 255, 0.25 * 255);
  for (float i = 0; i < width; i += grid.x) {
    line(i, 0, i, height);
  }
  for (float i = 0; i < height; i += grid.y) {
    line(0, i, width, i);
  }
}
