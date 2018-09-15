/* TODO
 * * Timings: ticks and check allowances
 * * Visuals:
 * * a Statistics
 * * b Holding block
 * * c Next block // Messed by bug 1
 *
/* Coordinate system
 * * 0 1 2 3 4 5 ... 9
 * 0                 |
 * 1                 | Height: 20
 * 2                 |
 * ...               V
 * 19 -------------->*
 *     Width: 10
 **/
// Constants

final int[][][] T_ORDS = { // [7][4][2] lengths
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

final T_type[] TETS = new T_type[] { // Constants dictionary
  new T_type('I', #00FFFF, T_ORDS[0]), 
  new T_type('O', #FFFF00, T_ORDS[1]), 
  new T_type('T', #800080, T_ORDS[2]), 
  new T_type('S', #00FF00, T_ORDS[3]), 
  new T_type('Z', #FF0000, T_ORDS[4]), 
  new T_type('L', #FFA500, T_ORDS[5]), 
  new T_type('J', #0000FF, T_ORDS[6])
};

// statistics functions
int[] T_stats = new int[7];
int score = 0;
int level = 0;
int addscore(int rows) {
  int x = level + 1;
  switch(rows) {
  case 1:
    return 40 * x;

  case 2:
    return 100 * x;

  case 3:
    return 300 * x;

  case 4:
    return 1200 * x;

  default:
    return 0;
  }
}

// Game
Tet player;
Matrix playfield;
int next = T_gen();
int held = -1; // Default to OutOfBounds index
boolean hold_enable = true;
PVector origin;
PVector dimentions;

void setup() {
  size(700, 600);
  origin = new PVector(width / 3, 10);
  dimentions = new PVector(width / 3, height - 20);
  reset();
}

void draw() {
  background(0);
  //frameRate(3); // makes controls very insensitive, or the game cruelly fast

  if (player.above_board() && playfield.query(player))
    lose();

  else if (player.update(playfield))
    player = new Tet(T_gen());

  player.show(playfield);
  playfield.show(dimentions);
  drawstats(width);
}

void keyPressed() {
  switch(key) {
  case 'w':
    hold();
    break;

  case 's':
    player.drop(playfield);
    break;

  case 'a':
    player.trans(-1, 0, false);
    break;

  case 'd':
    player.trans(1, 0, false);
    break;

  case 'e':
    player.rot(1, playfield);
    break;

  case 'q':
    player.rot(-1, playfield);
    break;

  case 'c':
    saveFrame("output.png");
    break;
  }

  switch(keyCode) { // Debugging controls
  case UP:
    player.trans(0, -1);
    break;
  case DOWN:
    player.trans(0, 1);
    break;
  }
}

int T_gen() {
  int current = next;
  next = floor(random(7));
  return current;
}

void hold() { // Swap Player with Held
  if (hold_enable) {
    int tmp = (held != -1) ? held : T_gen();
    held = player.type;
    player = new Tet(tmp);
    hold_enable = false;
  }
}

void reset() {
  player = new Tet(T_gen());
  playfield = new Matrix(10, 20, origin, dimentions);
}

void drawstats(float w) {
  float left = w / 3 - 10;//180;
  textAlign(RIGHT);

  String holdstr = "Held: ";
  holdstr += (held != -1) ? TETS[held].label : ' ';
  text(holdstr, left, 25);

  text("Next: " + TETS[next].label, left, 40);

  for (int i = 0; i < TETS.length; i++) {
    text(TETS[i].label + ": " + T_stats[i], left, 16 * i + 56);
  }

  text("Score: " + score, left, 168);
}

void win(int rows) {
  if (rows > 0) {
    println("win: " + rows);
    score += addscore(rows);
  }
}

void lose() {
  println("Lose");
  reset();
  // Hard reset
  score = 0;
  level = 0;
  for (int i = 0; i < T_stats.length; i++) {
    T_stats[i] = 0;
  }
}
