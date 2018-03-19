Table table;
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
  table = loadTable("table.tsv", "header");

  for (int i = 0; i < table.getRowCount(); i++) {
    TableRow row = table.getRow(i);

    int x_row = row.getInt("x");
    int y_row = row.getInt("y");
    int diam_row = row.getInt("diam");
    int hue_row = row.getInt("hue");
    String label_row = row.getString("label");

    circles.add(new Circle(x_row, y_row, diam_row, hue_row, label_row));
  }
}

void write() {
  TableRow row = table.addRow();
  Circle rcircle = new Circle(mouseX, mouseY);

  row.setInt("x", mouseX);
  row.setInt("y", mouseY);
  row.setInt("diam", rcircle.diam);
  row.setInt("hue", rcircle.hue);
  row.setString("label", rcircle.label);

  circles.add(rcircle);
}
