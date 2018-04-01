Container[][] squares = new Container[4][2]; // [cols][rows]

void setup() {
  size(600, 300);
}

void draw() {
  background(0);
  frameRate(2);

  for (int col = 0; col < squares.length; col++) {
    for (int row = 0; row < squares[col].length; row++) {
      float y = row;
      float x = col;

      y = map(y, 0, squares.length, 0, width);
      x = map(x, 0, squares[col].length, 0, height);

      squares[col][row] = new Container(x, y, 
        width / squares.length, height / squares[col].length);
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
