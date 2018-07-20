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
// Constants

final Pallete[] COLS = new Pallete[] { // Colour constants dictionary
  new Pallete(#555555, #FF0000), // Car
  new Pallete(#000066, #654321), // Log
  new Pallete(#007F00, #000000), //  Win
  new Pallete(#222222, #000000), // Safety
  new Pallete(#000000, #00FF00), // Frog
};

final int CAR = 0;
final int LOG = 1;
final int WIN = 2;
final int SAFETY = 3;
final int FROG = 4;

final int numTiles = 10;
final int maxLives = 3;

// Declarations
int lives = maxLives;
Lane[] lanes = new Lane[10];
Frog player;
PVector grid;

void setup() {
  size(400, 400);
  grid = new PVector(width / numTiles, height / lanes.length);
  reset();
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

void lose() { // Win/loss conditionals
  lives--;
  String Dstr = (lanes[player.myLane()].type == CAR) ?
    "You were ran over, " : "You drowned, ";
  Dstr += (lives > 0) ? lives + " lives remaining." : "game over.";
  println(Dstr);
  reset();
  if (lives < 1) { // Reset whole state after complete loss
    lives = maxLives;
  }
}

void win() {
  println("Win!");
  reset();
  lives = maxLives;
}

void reset() {
  player = new Frog();
  // Create lanes
  lanes = new Lane[] {
    new Lane(0), 
    new Lane(1, 4, -2, 0, CAR), 
    new Lane(2, 4, -2.5, 2, CAR), 
    new Lane(3, 4, -2, 0, CAR), 
    new Lane(4), 
    new Lane(5, 4, 1, 2, LOG), 
    new Lane(6, 4, 1.5, 0, LOG), 
    new Lane(7, 4, 1.52, 2, LOG), 
    new Lane(8), 
    new Lane(9), 
  };
  
  lanes[lanes.length - 1].col = COLS[WIN].lane; // Override winlane colour
}
