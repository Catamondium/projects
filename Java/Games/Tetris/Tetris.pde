// Constants
//int[] TYPES = {
//  0, // I
//  1, // O
//  2, // T
//  3, // S
//  4, // Z
//  5, // L
//  6}; // J

color[] T_COLS = {
  #00FFFF, 
  #FFFF00, 
  #800080, 
  #00FF00, 
  #FF0000, 
  #FFA500, 
  #0000FF};

int[][][] TETS = { // [7][4][2] lengths
  {{3, 0}, // I
    {6, 0}, 
    {4, 0}, //
  {5, 0}}, // Centres

  {{4, 0}, // O
    {5, 0}, 
    {4, 1}, 
  {5, 1}}, 

  {{3, 1}, // T
    {4, 0}, 
    {5, 1}, 
  {4, 1}}, // Centre

  {{4, 0}, // S
    {5, 0}, 
    {3, 1}, 
  {4, 1}}, // Centre

  {{3, 0}, // Z
    {4, 0}, 
    {5, 1}, 
  {4, 1}}, // Centre

  {{3, 0}, // L
    {3, 1}, 
    {5, 1}, 
  {4, 1}}, // Centre

  {{5, 0}, // J
    {3, 1}, 
    {5, 1}, 
  {4, 1}} // Centre
};

Tet player;
Matrix playfield;
int scale = 25;
PVector origin = new PVector(0, 0);
PVector dimentions = new PVector(10 * scale, 20 * scale);
void setup() {
  size(700, 600);
  player = new Tet(floor(random(6)), origin, dimentions);
  playfield = new Matrix(10, 20, origin, dimentions);
}

void draw() {
  background(0);
  //frameRate(3);
  //drawgrid(scale, playfield.w * scale, playfield.h * scale);
  if (player.update(scale, playfield))
    player = new Tet(floor(random(6)), origin, dimentions);
  playfield.show(scale);
  player.show(scale);
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
    player.trans(-1, 0);
    break;

  case 'd':
    player.trans(1, 0);
    break;

  case 'e':
    player.rot();
    break;

  case 'c':
    saveFrame("output.png");
    break;
  }
}

void drawgrid(int scale, int w, int h) {
  noFill();
  stroke(255);
  for (int x = 0; x < w; x++) {
    for (int y = 0; y < h; y++) {
      rect(x * scale, y * scale, scale, scale);
    }
  }
}
