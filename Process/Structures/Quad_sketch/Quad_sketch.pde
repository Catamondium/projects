Rectangle boundary;
QuadTree tree;

void setup() {
  size(500, 400);

  boundary = new Rectangle(width / 2, height / 2, width / 2, height / 2);
  tree = new QuadTree(boundary, 4);
  for (int i = 0; i < 250; i++) {
    PVector p = new PVector(random(width), random(height));
    tree.insert(p);
  }
}

void draw() {
  background(0);
  tree.show();

  stroke(0, 255, 0);
  strokeWeight(1);
  rectMode(CENTER);

  Rectangle range = new Rectangle(mouseX, mouseY, 50, 50);
  rect(range.x, range.y, range.w * 2, range.h * 2);

  ArrayList<PVector> points = new ArrayList<PVector>();
  tree.query(range, points);

  for (PVector p : points) {
    stroke(0, 0, 255);
    strokeWeight(4);
    point(p.x, p.y);
  }

  if (mousePressed) {
    for (int i = 0; i < 5; i++) {
      PVector p = new PVector(mouseX + random(-5, 5), mouseY + random(-5, 5));
      tree.insert(p);
    }
  }
}
