void setup() {
  size(100, 100);
  colorMode(HSB, width);
}
int[] data = {50, 82, 20, 65, 45, 80, 90, 100, 95, 50};
float divisions = width / data.length;
int breakvar = 0;
void draw() {
  background(0);

  for (int i = 0; i < data.length; i++) {
    fill(i*divisions, 255, 255);
    rect(0, i * divisions, data[i], divisions);
  }

  if (breakvar == 0) {
    saveFrame("output.png"); 
    breakvar++;
  }
}
