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
      float cell_w = w / cells.length;
      float cell_h = h / cells[col].length;
      float cell_x = map(col, 0, cells[col].length, 0, h);
      for (int row = 0; row < cells[col].length; row++) {
        float cell_y = map(row, 0, cells.length, 0, w);
        translate((0.5 * cell_w), (0.5 * cell_h));
        cells[col][row] = new Cell(cell_x, cell_y, cell_w, cell_h);
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
