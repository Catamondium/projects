/*GAMEPLAY:
 * Car ==> deadly intersection
 * Log ==> sticky intersection, deadly !intersection
 *
 *BUG:
 * frog.count resetting at winlane :: GameReset() responsible.
 **/

Frog player;
PVector grid;
Lane[] lanes = new Lane[10];
int lives = 3;

void GameOver() { // Win/loss conditionals
  if (player.myLane() == lanes.length) {
    println("Win!");
  } else {
    lives--;
    if (lives > 0) {
      println("You have " + lives + " lives remaining");
    } else {
      println("Game over");
    }
  }
  GameReset();
}
void GameReset() {
  player = new Frog(); // Why no respawn?
  // Create lanes
  lanes[0] = new Lane(0);
  lanes[1] = new Lane(1, 4, -2, 0, CAR);
  lanes[2] = new Lane(2, 4, -2.5, 2, CAR);
  lanes[3] = new Lane(3, 4, -2, 0, CAR);
  lanes[4] = new Lane(4);
  lanes[5] = new Lane(5, 4, 1, 2, LOG);
  lanes[6] = new Lane(6, 4, 1.5, 0, LOG);
  lanes[7] = new Lane(7, 4, 1.52, 2, LOG);
  lanes[8] = new Lane(8);
  lanes[9] = new Lane(9);
}

void setup() {
  size(400, 400);
  noSmooth();
  grid = new PVector(40, height / lanes.length); // 10 lanes, 10 tiles long
  GameReset();
}

void draw() {
  background(0);
  for (Lane a : lanes) {
    a.update();
    a.display();
  }
  player.update();
  player.show();


  lanes[player.myLane()].check(player);
  //debug();
}

void keyPressed() {
  switch(key) {
  case 'w':
    player.dir(0, -1);
    break;
  case 's':
    player.dir(0, 1);
    break;
  case 'a':
    player.dir(-1, 0);
    break;
  case 'd':
    player.dir(1, 0);
    break;
  }
}

void debug() {
  stroke(255, 0, 255, 0.30 * 255);
  for (float i = 0; i < width; i += grid.x) {
    line(i - 1, 0, i - 1, height);
  }
  for (float i = 0; i < height; i += grid.y) {
    line(0, i - 1, width, i - 1);
  }
}
