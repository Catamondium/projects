ArrayList<Drone> drones = new ArrayList<Drone>();

void setup() {
  size(600, 600);
  colorMode(HSB, 360);
}

void draw() {
  background(0);
  PVector mouse = new PVector(mouseX, mouseY);
  fill(50);
  ellipse(mouse.x, mouse.y, 24, 24);
  for (Drone all : drones) {
    all.update();
    all.show();
  }
}

void mousePressed() {
  for (int i = 0; i < 5; i++) {
    drones.add(new Drone(mouseX + floor(random(-10, 10)), mouseY + floor(random(-10, 10))));
  }
}
