class Oblong {
  int x;
  int y;
  int w;
  int h;

  Oblong(int _x, int _y, int _w, int _h) {
    x = _x;
    y = _y;
    w = _w;
    h = _h;
  }

  void show() {
    fill(255);
    rect(x, y, w, h);
  }
}
