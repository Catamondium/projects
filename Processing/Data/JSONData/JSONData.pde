JSONObject data;
JSONArray dataArray;
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
  data = loadJSONObject("data.json");
  dataArray = data.getJSONArray("circles");

  for (int i = 0; i < dataArray.size(); i++) {
    JSONObject r_circle = dataArray.getJSONObject(i).getJSONObject("circle");

    int data_x = r_circle.getJSONObject("pos").getInt("x");
    int data_y = r_circle.getJSONObject("pos").getInt("y");
    int data_diam = r_circle.getInt("diameter");
    int data_hue = r_circle.getInt("hue");
    String data_label = r_circle.getString("label");

    circles.add(new Circle(data_x, data_y, data_diam, data_hue, data_label));
  }
}

void write(Circle entity) {
  JSONObject w_circle = new JSONObject();
  JSONObject w_circleA = new JSONObject();

  JSONObject pos = new JSONObject();

  pos.setInt("x", entity.x);
  pos.setInt("y", entity.y);
  w_circleA.setJSONObject("pos", pos);

  w_circleA.setInt("diameter", entity.diam);
  w_circleA.setInt("hue", entity.hue);
  w_circleA.setString("label", entity.label);

  w_circle.setJSONObject("circle", w_circleA);

  dataArray.setJSONObject(dataArray.size(), w_circle);

  if (circles.size() > 10) {
    int item = floor(random(0, circles.size()));
    dataArray.remove(item);
    circles.remove(item);
  }
  saveJSONObject(data, "data/data.json");
}

void mousePressed() {
  Circle newcircle = new Circle(mouseX, mouseY);
  circles.add(newcircle);
  write(newcircle);
}
