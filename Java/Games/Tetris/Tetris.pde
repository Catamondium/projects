/*TODO:
 * * Gameplay: 
 * * * Scoring system
 * * Timings: ticks and check allowances
 **/
/* Coordinate system
 * * 0 1 2 3 4 5 ...9
 * 0                |
 * 1                | Height: 20
 * 2                |
 * ...              V
 * 19 ------------->*
 *     Width: 10
 **/
// Constants
color[] T_COLS = {
  #00FFFF, 
  #FFFF00, 
  #800080, 
  #00FF00, 
  #FF0000, 
  #FFA500, 
  #0000FF};

int[][][] TETS = { // [7][4][2] lengths
  {{3, -2}, // I
    {6, -2}, 
    {4, -2}, //
  {5, -2}}, // Centres

  {{4, -3}, // O
    {5, -3}, 
    {4, -2}, 
  {5, -2}}, 

  {{3, -2}, // T
    {4, -3}, 
    {5, -2}, 
  {4, -2}}, // Centre

  {{4, -3}, // S
    {5, -3}, 
    {3, -2}, 
  {4, -2}}, // Centre

  {{3, -3}, // Z
    {4, -3}, 
    {5, -2}, 
  {4, -2}}, // Centre

  {{3, -3}, // L
    {3, -2}, 
    {5, -2}, 
  {4, -2}}, // Centre

  {{5, -3}, // J
    {3, -2}, 
    {5, -2}, 
  {4, -2}} // Centre
};

//int[] stats = {
//  0, // I
//  1, // O
//  2, // T
//  3, // S
//  4, // Z
//  5, // L
//  6}; // J

Tet player;
Matrix playfield;
int scale = 25;
PVector origin;
PVector dimentions;
void setup() {
  size(700, 600);
  origin = new PVector(width / 3, 10);
  dimentions = new PVector(width / 3, height - 20);//200, 400);
  reset();
}

void draw() {
  background(0);
  //frameRate(3);

  if (player.above_board() && playfield.query(player)) {
    println("Lose");
    reset();
  } else if (player.update(playfield)) {
    player = new Tet(floor(random(6)));
  }

  playfield.show(dimentions);
  player.show(playfield);
  //noLoop();
}

void keyPressed() {
  switch(key) {
  case 'w':
    player.trans(0, -1);
    break;

  case 's':
    player.trans(0, 1);
    break;

  case 'a':
    player.trans(-1, 0, false);
    break;

  case 'd':
    player.trans(1, 0, false);
    break;

  case 'e':
    player.rot(1);
    break;

  case 'q':
    player.rot(-1);
    break;

  case 'f':
    player.drop(playfield);
    break;

  case 'c':
    saveFrame("output.png");
    break;
  }
}

void reset() {
  player = new Tet(floor(random(6)));
  playfield = new Matrix(10, 20, origin, dimentions);
}

void win(int rows) {
  println("win: " + rows);
}
