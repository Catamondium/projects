Container cont;

void setup() {
  size(600, 600);
  cont = new Container(0, 0, width, height, 4, 2);
}

void draw() {
  background(0);
  frameRate(2);
  cont.update();
  cont.show();
  //saveFrame("output.png"); 
  //noLoop();
}
