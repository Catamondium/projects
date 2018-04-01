Container[][] squares = new Container[4][2]; // [cols][rows]

void setup() {
  size(600, 300);
}

void draw() {
  background(0);
  frameRate(2);

  for (int col = 0; col < squares.length; col++) {
    float w = width / squares.length;
    float h = height / squares[col].length;
    float x = map(col, 0, squares[col].length, 0, height);
    for (int row = 0; row < squares[col].length; row++) {
      float y = map(row, 0, squares.length, 0, width);
      squares[col][row] = new Container(x, y, w, h);
    }
  }

  for (Container[] col : squares) {
    for (Container row : col) {
      row.update();
      row.show();
    }
  }
  //saveFrame("output.png"); 
  //noLoop();
}
