Frog frog;
Obsticle obj;
PVector grid;
Lane[] lanes = new Lane[2];

void ResetGame() {
  frog = new Frog(0.5 * width);
}

void setup() {
  size(450, 550);
  grid = new PVector(50, 50); // 11 lanes, 11 tiles long
  ResetGame();
  obj = new Obsticle(width / 2, height - (1 * grid.y) - (0.5 * grid.y), 2 * grid.x, -2, #FF0000);
  lanes[0] = new Lane(height - (1 * grid.y), #001100);
  lanes[1] = new Lane(height - (2 * grid.y), 4, 2, -2, 0, #110000, CAR);
}

void draw() {
  background(0);
  obj.update();
  frog.update();
  //lanes[0].update();
  //lanes[1].update();
  //lanes[0].display();
  //lanes[1].display();
  obj.show();
  frog.show();
  debug();
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
