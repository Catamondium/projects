class Oblong {
  int x;
  int y;
  int w;
  int h;

  Oblong(int x_, int y_, int w_, int h_) {
    x = x_;
    y = y_;
    w = w_;
    h = h_;
  }

  void show() {
    fill(255);
    rect(x, y, w, h);
  }
}
