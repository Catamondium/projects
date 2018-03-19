Table data;
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
    if (d < entity.diam) {
      entity.over = true;
    } else {
      entity.over = false;
    }

    entity.show();
  }

  //saveFrame();
  //noLoop();
}

void read() {
  data = loadTable("data.tsv", "header");

// ****** Read XML ******

    circles.add(new Circle(x_row, y_row, diam_row, hue_row, label_row));
  }
}

void write(Circle entity) {
// ****** Write XML ******
}

void mousePressed() {
  Circle newcircle = new Circle(mouseX, mouseY);
  circles.add(newcircle);
  write(newcircle);
}
