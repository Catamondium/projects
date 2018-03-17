Table csv;
ArrayList<Oblong> oblongs = new ArrayList<Oblong>();

void setup() {
  size(600, 600);
  loadCSV();
}

void draw() {
  background(0);

  for (Oblong entity : oblongs) {
    entity.show();
  }

  saveFrame();
  noLoop();
}

void loadCSV() {
  csv = loadTable("test.csv", "header");

  for (int i = 0; i < csv.getRowCount(); i++) {
    TableRow row = csv.getRow(i);

    int x_row = row.getInt("x");
    int y_row = row.getInt("y");
    int w_row = row.getInt("width");
    int h_row = row.getInt("height");

    oblongs.add(new Oblong(x_row, y_row, w_row, h_row));
  }
}
