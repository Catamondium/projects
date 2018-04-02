class Container {
  float x, y, w, h;
  Cell[][] cells;

  Container(float x_, float y_, float w_, float h_, int cols_, int rows_) { // Drawn relative to center
    x = x_ - (0.5 * w_);
    y = y_ - (0.5 * h_);
    w = w_;
    h = h_;
    cells = new Cell[cols_][rows_];
  }

  void update() {
    for (int col = 0; col < cells.length; col++) {
      translate(x, y);
      float wCell = w / cells.length;
      float hCell = h / cells[col].length;
      float xCell = map(col, 0, cells[col].length, 0, h);
      for (int row = 0; row < cells[col].length; row++) {
        float yCell = map(row, 0, cells.length, 0, w);
        cells[col][row] = new Cell(xCell, yCell, wCell, hCell);
      }
    }
  }

  void show() {
    for (Cell[] col : cells) {
      for (Cell row : col) {
        row.update();
        row.show();
      }
    }
  }
}
