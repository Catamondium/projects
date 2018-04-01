Container[][] squares = new Container[2][4]; // [x][y]

void setup() {
  size(600, 300);
}

void draw() {
  background(0);
  frameRate(2);

  for (int x = 0; x < squares.length; x++) {
    for (int y = 0; y < squares[x].length; y++) {
      float col = x;
      float row = y;

      col = map(col, 0, squares.length, 0, height);
      row = map(row, 0, squares[x].length, 0, width);

      squares[x][y] = new Container(row, col, width / squares[x].length, height / squares.length);
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
