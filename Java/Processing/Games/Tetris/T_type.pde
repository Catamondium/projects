class T_type { // Object to form dictionary of block constants
  char label;
  color col;
  PVector[] ords = new PVector[4];

  T_type(char lab_, color col_, int[][] ords_) {
    label = lab_;
    col = col_;
    for (int i = 0; i < 4; i++) {
      ords[i] = new PVector(ords_[i][0], ords_[i][1]);
    }
  }
}
