Container[] batch;

void setup() {
  size(700, 600);
  batch = new Container[2];
  PVector[] pos = new PVector[2];
  pos[0] = new PVector(width / 4, height / 4);
  pos[1] = new PVector((3 * width) / 4, (3 * height) / 4);
  batch[0] = new Container(pos[0], width / 2, height / 2, 4, 4);
  batch[1] = new Container(pos[1], width / 2, height / 2, 4, 1);
}

void draw() {
  background(0);
  frameRate(2);
  for (Container a : batch) {
    a.update();
    a.show();
  }
  
  //save("output.png");
  //noLoop();
}
