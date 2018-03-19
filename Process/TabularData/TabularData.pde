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
  data = loadTable("table.tsv", "header");

  for (int i = 0; i < data.getRowCount(); i++) {
    TableRow row = data.getRow(i);

    int x_row = row.getInt("x");
    int y_row = row.getInt("y");
    int diam_row = row.getInt("diam");
    int hue_row = row.getInt("hue");
    String label_row = row.getString("label");

    circles.add(new Circle(x_row, y_row, diam_row, hue_row, label_row));
  }
}

void write(Circle entity) {
  TableRow row = data.addRow();

  row.setInt("x", entity.x);
  row.setInt("y", entity.y);
  row.setInt("diam", entity.diam);
  row.setInt("hue", entity.hue);
  row.setString("label", entity.label);
  saveTable(data, "data/table.tsv");
}

void mousePressed() {
  Circle newcircle = new Circle(mouseX, mouseY);
  circles.add(newcircle);
  write(newcircle);
}
