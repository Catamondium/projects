/*TODO:
 * GAMEPLAY:
 *  Log ==> sticky intersection, deadly !intersection
 *  Car ==> deadly intersection
 **/

Frog frog;
PVector grid;
Lane[] lanes = new Lane[10];

void ResetGame() {
  frog = new Frog();
}

void setup() {
  size(400, 400);
  noSmooth();
  grid = new PVector(40, height / lanes.length); // 10 lanes, 10 tiles long
  ResetGame();

  // Create lanes
  lanes[0] = new Lane(0);
  lanes[1] = new Lane(1, 4, -2, 0, CAR);
  lanes[2] = new Lane(2, 4, -2, 2, CAR);
  lanes[3] = new Lane(3, 4, -2, 0, CAR);
  lanes[4] = new Lane(4);
  lanes[5] = new Lane(5, 4, -2, 2, LOG);
  lanes[6] = new Lane(6, 4, -2, 0, LOG);
  lanes[7] = new Lane(7, 4, -2, 2, LOG);
  lanes[8] = new Lane(8);
  lanes[9] = new Lane(9);
}

void draw() {
  background(0);
  for (Lane a : lanes) {
    a.update();
    a.display();
  }
  frog.update();
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

void debug() {
  stroke(255, 255, 255, 0.25 * 255);
  for (float i = 0; i < width; i += grid.x) {
    line(i - 1, 0, i - 1, height);
  }
  for (float i = 0; i < height; i += grid.y) {
    line(0, i - 1, width, i - 1);
  }
}
