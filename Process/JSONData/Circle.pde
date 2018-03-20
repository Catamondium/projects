class Circle {
  int x;
  int y;
  int diam;

  int hue;
  String label;

  boolean over = false;

  Circle(int x_, int y_, int diam_, int hue_, String label_) {
    x = x_;
    y = y_;
    diam = diam_;
    hue = hue_;
    label = label_;
  }

  Circle(int x_, int y_) {
    x = x_;
    y = y_;
    diam = floor(random(20, 100));
    hue = floor(random(10, 350));
    label = "New circle";
  }

  void show() {
    fill(hue, 360, 360);
    ellipse(x, y, diam, diam);

    if (over) {
      textAlign(CENTER);
      fill(360);
      text(label, x, y);
    }
  }
}
