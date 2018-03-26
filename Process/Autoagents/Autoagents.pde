ArrayList<Vehicle> vehicles = new ArrayList<Vehicle>();

void setup() {
  size(600, 600);
}

void draw() {
  background(0);
  PVector mouse = new PVector(mouseX, mouseY);
  fill(50);
  ellipse(mouse.x, mouse.y, 24, 24);
  for (Vehicle all : vehicles) {
    all.seek(mouse);
    all.arrive(mouse);
    all.update();
    all.show();
  }
}

void mousePressed() {
  vehicles.add(new Vehicle(mouseX, mouseY));
}
