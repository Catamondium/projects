JSONObject data;
ArrayList<Circle> circles = new ArrayList<Circle>();

void setup() {
  size(600, 600);
  colorMode(HSB, 360);
  read();
}

void draw() {
  background(0);

  for (Circle entity : circles) {
    float d = dist(entity.x, entity.y, mouseX, mouseY);
    if (d < 0.5 * entity.diam) {
      entity.over = true;
    } else {
      entity.over = false;
    }

    entity.show();
  }

  noLoop();
}

void read() {
  data = loadJSONObject("data.json");

  for (int i = 0; i < array; i++) {
    // JSON read
    circles.add(new Circle(x_row, y_row, diam_row, hue_row, label_row));
  }
}

void write(Circle entity) {
  //JSON write

  if (circles.size() > 10) {
    int item = floor(random(0, circles.size()));
    // JSON delete
    circles.remove(item);
  }
  saveJSONObject(data, "data/data.json");
}

void mousePressed() {
  Circle newcircle = new Circle(mouseX, mouseY);
  circles.add(newcircle);
  write(newcircle);
  redraw();
}
