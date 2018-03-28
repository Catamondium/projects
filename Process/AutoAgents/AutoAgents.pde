ArrayList<Drone> drones = new ArrayList<Drone>();

void setup() {
  size(600, 600);
  colorMode(HSB, 360);

  // Inital spawn
  for (int i = 0; i < 225; i++) {
    Drone unit = new Drone(random(width), random(height));
    drones.add(unit);
  }
}

void draw() {
  background(0);
  fill(50);
  //ellipse(mouse.x, mouse.y, 24, 24);
  for (Drone all : drones) {
    all.update();
    all.show();
  }
  textAlign(LEFT);
  fill(360);
  text(("Framerate: " + floor(frameRate)), 0, 15);

  //saveFrame("output.png");
  //noLoop();
}

void mousePressed() {
  for (int i = 0; i < 5; i++) {
    drones.add(new Drone(mouseX + floor(random(-10, 10)), mouseY + floor(random(-10, 10))));
  }
}
