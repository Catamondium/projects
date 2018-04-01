Container[][] squares = new Container[2][4]; // [x][y]
void setup() {
  size(600, 300);
}

void draw() {
  background(0);
  frameRate(2);

  for (int x = 0; x < squares.length; x++) {
    for (int y = 0; y < squares[x].length; y++) {
      float column = x;
      float row = y;
      column = map(column, 0, squares.length, 0, height);
      row = map(row, 0, squares[x].length, 0, width);

      squares[x][y] = new Container(row, column, width/squares[x].length, height/squares.length);
    }
  }

  for ( Container[] s : squares) {
    for (Container a : s) {
      a.update();
      a.show();
    }
  }
  //saveFrame("output.png"); 
  //noLoop();
}
