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
    if (d < 0.5 * entity.diam) {
      entity.over = true;
    } else {
      entity.over = false;
    }

    entity.show();
  }
}

void read() {
  data = loadTable("data.tsv", "header");

  for (int i = 0; i < data.getRowCount(); i++) {
    TableRow row = data.getRow(i);

    int data_x = row.getInt("x");
    int data_y = row.getInt("y");
    int data_diam = row.getInt("diam");
    int data_hue = row.getInt("hue");
    String data_label = row.getString("label");

    circles.add(new Circle(data_x, data_y, data_diam, data_hue, data_label));
  }
}

void write(Circle entity) {
  TableRow row = data.addRow();

  row.setInt("x", entity.x);
  row.setInt("y", entity.y);
  row.setInt("diam", entity.diam);
  row.setInt("hue", entity.hue);
  row.setString("label", entity.label);

  if (circles.size() > 10) {
    int item = floor(random(0, circles.size()));
    data.removeRow(item);
    circles.remove(item);
  }
  saveTable(data, "data/data.tsv");
}

void mousePressed() {
  Circle newcircle = new Circle(mouseX, mouseY);
  circles.add(newcircle);
  write(newcircle);
}
