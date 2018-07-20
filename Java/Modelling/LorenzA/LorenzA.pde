final float sigma = 10;
final float beta = 8.0/3.0;
final float rho = 28;

PVector ord = new PVector(1, 1, 1);
PVector diff = new PVector();

ArrayList<PVector> points = new ArrayList<PVector>();
void setup() {
  size(800, 600);
  colorMode(HSB);
}

void draw() {
  background(0);
  final float dt = 0.01;
  diff.x = sigma * (ord.y - ord.x);
  diff.y = ord.x * (rho - ord.z) - ord.y;
  diff.z = ord.x * ord.y - beta * ord.z;
  diff.mult(dt);

  ord.add(diff);

  points.add(new PVector(ord.x, ord.y, ord.z));

  translate(width/2, height/2);
  scale(5);
  stroke(255);
  noFill();

  float hu = 0;
  beginShape();
  for (PVector v : points) {
    stroke(hu, 255, 255);
    vertex(v.x, v.y);
    hu = hu + 0.1;
    if (hu > 255) {
      hu = 0;
    }
  }
  endShape();
}
