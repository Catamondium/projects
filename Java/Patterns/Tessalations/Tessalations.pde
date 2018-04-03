Tessalator[] tessalators = new Tessalator[2];
int count = 0;

void setup() {
  size(700, 600);
  PVector[] centers = new PVector[2];
  centers[0] = new PVector(0.25 * width, 0.25 * height);
  centers[1] = new PVector(0.75 * width, 0.75 * height);
  for (int i = 0; i < tessalators.length; i++) {
    tessalators[i] = new Tessalator(centers[i], width / 2, height / 2);
  }
}

void draw() {
  background(0);
  frameRate(2);
  for (Tessalator tes : tessalators) {
    tes.update();
    tes.show();
  }

  //save("output.png");
  //noLoop();
}

void mousePressed() {
  if (count < 4) {
    for (Tessalator tes : tessalators) {
      tes.tessalate();
    }
    count++;
  }
}
