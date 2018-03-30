Hive hive;

void setup() {
  size(1200, 600);
  colorMode(HSB, 360);
  hive = new Hive();
  // Inital spawn
  for (int i = 0; i < 200; i++) {
    Drone unit = new Drone(random(width), random(height));
    hive.addDrone(unit);
  }
}

void draw() {
  background(0);
  fill(50);
  //ellipse(mouse.x, mouse.y, 24, 24);
  hive.run();
  textAlign(LEFT);
  fill(360);
  text(("Framerate: " + floor(frameRate)), 0, 15);

  //saveFrame("output.png");
  //noLoop();
}

void mousePressed() {
  for (int i = 0; i < 5; i++) {
    hive.addDrone(new Drone(mouseX + floor(random(-10, 10)), mouseY + floor(random(-10, 10))));
  }
}
