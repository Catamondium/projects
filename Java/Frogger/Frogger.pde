Frog frog;
Obsticle obj;
PVector grid;
Lane[] lanes = new Lane[5];

void ResetGame() {
  frog = new Frog(6);
}

void setup() {
  size(400, 400);
  grid = new PVector(40, 40); // 10 lanes, 10 tiles long
  lanes = new Lane[floor(height / grid.y)];
  ResetGame();
  lanes[0] = new Lane(0, #222222);
  lanes[1] = new Lane(1, 4, 2, -2, 1, #555555, CAR);
  lanes[2] = new Lane(2, 4, 2, -2, 1, #555555, CAR);
  lanes[3] = new Lane(3, 4, 2, -2, 1, #555555, CAR);
  lanes[4] = new Lane(4, #222222);
  lanes[5] = new Lane(5, 4, 2, -2, 1, #000033, LOG);
  lanes[6] = new Lane(6, 4, 2, -2, 1, #000033, LOG);
  lanes[7] = new Lane(7, 4, 2, -2, 1, #000033, LOG);
  lanes[8] = new Lane(8, #222222);
  lanes[9] = new Lane(9, #222222);
}

void draw() {
  background(0);
  for (Lane a : lanes) {
    a.update();
    a.display();
  }
  frog.update();
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
