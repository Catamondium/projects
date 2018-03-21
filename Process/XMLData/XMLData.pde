XML data;
XML[] dataArray;
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
  data = loadXML("data.xml");
  dataArray = data.getChildren("circle");
  for (int i = 0; i < dataArray.length; i++) {

    XML r_x = dataArray[i].getChild("pos/x");
    int data_x = r_x.getIntContent();

    XML r_y = dataArray[i].getChild("pos/y");
    int data_y = r_y.getIntContent();

    XML r_diam = dataArray[i].getChild("diameter");
    int data_diam = r_diam.getIntContent();

    XML r_hue = dataArray[i].getChild("hue");
    int data_hue = r_hue.getIntContent();

    String data_label = dataArray[i].getContent(); // *** Bugged, but avoids null pointer exception

    circles.add(new Circle(data_x, data_y, data_diam, data_hue, data_label));
  }
}

void write(Circle entity) {
  XML newcircle = data.addChild("circle");
  XML position = newcircle.addChild("pos");

  XML w_x = position.addChild("x");
  w_x.setIntContent(entity.x);

  XML w_y = position.addChild("y");
  w_y.setIntContent(entity.y);

  XML w_diam = newcircle.addChild("diameter");
  w_diam.setIntContent(entity.diam);

  XML w_hue = newcircle.addChild("hue");
  w_hue.setIntContent(entity.hue);

  XML w_label = newcircle.addChild("label");
  w_label.setContent(entity.label);


  if (circles.size() > 10) {
    int item = floor(random(0, circles.size()));
    data.removeChild(dataArray[item]);
    circles.remove(item);
  }
  saveXML(data, "data/data.xml");
}

void mousePressed() {
  Circle newcircle = new Circle(mouseX, mouseY);
  circles.add(newcircle);
  write(newcircle);
}
