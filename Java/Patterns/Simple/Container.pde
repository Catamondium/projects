class Container {
  PVector pos;
  float w, h;
  Cell[][] cells;

  Container(PVector pos_, float w_, float h_, int col, int row) {
    pos = pos_;
    w = w_;
    h = h_;
    cells = new Cell[col][row];
  }

  void gen() {
    float cell_w = w / cells.length;
    for (int col = 0; col < cells.length; col++) {
      float cell_x = map(col, 0, cells.length, pos.x - 0.5 * (w - cell_w), 
        pos.x + 0.5 * (w + cell_w));
      float cell_h = h / cells[col].length;
      for (int row = 0; row < cells[col].length; row++) {
        float cell_y = map(row, 0, cells[col].length, pos.y - 0.5 * (h - cell_h), 
          pos.y + 0.5 * (h + cell_h));
        PVector cell_pos = new PVector(cell_x, cell_y);
        cells[col][row] = new Cell(cell_pos, cell_w, cell_h);
      }
    }
  }

  void update() {
    gen();
    for (Cell[] col : cells) {
      for (Cell row : col) {
        row.update();
      }
    }
  }

  void show() {
    pushStyle();
    for (Cell[] col : cells) {
      for (Cell row : col) {
        row.show();
      }
    }

    rectMode(CENTER);
    noFill();
    colorMode(RGB, 255);
    strokeWeight(2);
    stroke(255, 0, 0);
    rect(pos.x, pos.y, w, h);
    popStyle();
  }
}
