Hive hive;

void setup() {
  size(800, 600);
  colorMode(HSB, 360);
  Rectangle boundary = new Rectangle(width / 2, height / 2, width / 2, height / 2);
  hive = new Hive(boundary);
  // Inital spawn
  for (int i = 0; i < 250; i++) {
    Drone unit = new Drone(random(width), random(height));
    hive.addDrone(unit);
  }
}

void draw() {
  background(0);
  hive.run();

  textAlign(LEFT);
  fill(125, 360, 360, 720);
  text(("Framerate: " + floor(frameRate) + "\n" + "Units: " + hive.drones.size()), 3, 15);

  if (mousePressed) {
    for (int i = 0; i < 5; i++) {
      hive.addDrone(new Drone(mouseX + floor(random(-10, 10)), mouseY + floor(random(-10, 10))));
    }
  }

  //saveFrame("output.png");
  //noLoop();
}
