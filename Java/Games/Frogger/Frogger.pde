/*
 * * 0 1 2 3 4 5 6 |
 * 6               |
 * 5               |
 * 4 Board         | Height
 * 3 Coordinates   |
 * 2               |
 * 1               |
 * 0               |
 * --------------->V
 *      Width
 **/

Frog player;
PVector grid;
int numTiles = 10;
Lane[] lanes = new Lane[10];
int maxLives = 3;
int lives = maxLives;

void setup() {
  size(400, 400);
  grid = new PVector(width / numTiles, height / lanes.length); // 10 lanes, 10 tiles long
  GameReset();
}

void draw() {
  background(0);
  player.update();
  for (Lane a : lanes) {
    a.run();
  }
  lanes[player.myLane()].check(player);
  player.show();
  textAlign(LEFT);
  text("Lives: " + lives, 2, 12);

  //save("output.png");
  //noLoop();
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
  stroke(255, 255, 255, 0.30 * 255);
  for (float i = 0; i < width; i += grid.x) {
    line(i - 1, 0, i - 1, height);
  }
  for (float i = 0; i < height; i += grid.y) {
    line(0, i - 1, width, i - 1);
  }
}

void GameOver() { // Win/loss conditionals
  lives--;
  String deathmethod = "";
  switch(lanes[player.myLane()].type) {
  case 1:
    deathmethod += "You got run over, ";
    break;
  case 2:
    deathmethod += "Your drowned, ";
  }
  if (lives > 0) {
    println(deathmethod + lives + " lives remaining.");
  } else {
    println(deathmethod + "Game over");
  }
  GameReset();
  if (lives < 1) { // Reset whole state after complete loss
    lives = maxLives;
  }
}

void GameWon() {
  println("Win!");
  GameReset();
  lives = maxLives;
}

void GameReset() {
  player = new Frog();
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
  lanes[9].col = #003300; // Override winlane colour
}
