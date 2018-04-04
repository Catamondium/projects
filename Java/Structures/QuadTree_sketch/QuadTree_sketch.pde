Rectangle boundary;
QuadTree tree;

void setup() {
  size(1200, 600);
  colorMode(RGB, 255);
  rectMode(CENTER);
  noFill();
  strokeWeight(1);
  boundary = new Rectangle(width / 2, height / 2, width / 2, height / 2);
}

void draw() {
  tree = new QuadTree(boundary, 4);
  for (int i = 0; i < 600; i++) {
    PVector p = new PVector(random(width), random(height));
    tree.insert(p);
  }
  background(0);
  frameRate(0.5);
  tree.debug(0);


  // Testing
  for (int i = 0; i < 3; i++) {
    quadrat(random(width), random(height), random(100));
  }
  //if (mousePressed) {
  //  for (int i = 0; i < 5; i++) {
  //    PVector p = new PVector(mouseX + random(-5, 5), mouseY + random(-5, 5));
  //    tree.insert(p);
  //  }
  //}
  
  //save("output.png");
  //noLoop();
}

void quadrat(float x, float y, float dist_) {
  pushStyle();
  Rectangle range = new Rectangle(x, y, dist_, dist_);
  ArrayList<PVector> results = new ArrayList<PVector>();
  tree.query(range, results);

  stroke(0, 255, 0);
  rect(x, y, dist_ * 2, dist_ * 2);

  colorMode(HSB, 360);
  for (PVector b : results) {
    float d = dist(x, y, b.x, b.y);
    d = map(d, 0, dist_, 0, 360);
    stroke(d, 360, 360);
    strokeWeight(4);
    point(b.x, b.y);
  }
  popStyle();
}
