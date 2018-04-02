Container cont;

void setup() {
  size(600, 600);
  cont = new Container(300, 300, 300, 400, 1, 1);
}

void draw() {
  background(0);
  frameRate(2);
  cont.update();
  cont.show();
  //saveFrame("output.png"); 
  //noLoop();
}
