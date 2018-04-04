Frog frog;
Obsticle obj;
PVector grid;

void ResetGame() {
  println("game over");
  frog = new Frog(0.5 * (width - grid.x));
}
void setup() {
  size(550, 550);
  grid = new PVector(50, 50); // 11 lanes
  ResetGame();
  obj = new Obsticle(0.5 * (width - grid.x), height + (2 * -grid.y), grid.x * 2, -2, #FF0000);
}

void showgrid() {
  stroke(255, 255, 255, 0.25 * 255);
  for (float i = 0; i < width; i += grid.x) {
    line(i, 0, i, height);
  }
  for (float j = 0; j < height; j += grid.y) {
    line(0, j, width, j);
  }
}

void draw() {
  background(0);
  obj.update();
  obj.show();
  frog.show();
  showgrid();
}

void keyPressed() {
  switch(key) {
  case 'w':
    frog.dir(0, -1);
    frog.lane++;
    break;
  case 's':
    frog.dir(0, 1);
    frog.lane--;
    break;
  case 'a':
    frog.dir(-1, 0);
    break;
  case 'd':
    frog.dir(1, 0);
    break;
  }
}
