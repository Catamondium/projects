Layout ui;

void setup() {
  size(500, 500);

  LayoutBuilder uib = new LayoutBuilder(2, 2);
  uib.setGeo(10, 10, 350, 350);

  uib.addButton(new BClick() {
    public void onPress() {
      println("BCLICK_1");
    }
  })
  .addButton(new BClick() {
    public void onPress() {
      println("BCLICK_2");
    }
  })
  .addSlider(new SClick() {
    public void consume(Float val) {
      println("TClick", val);
    }
  })
  .addUI(new LayoutBuilder(2)
    .addToggle(new TClick() {
      public void consume(Boolean b) {
        println("SUB_1", b);
      }
    })
    .addToggle(new TClick() {
      public void consume(Boolean b) {
        println("SUB_2", b);
      }
    }).build());

  ui = uib.build();
}

void draw() {
  background(0);
  image(ui.render(), 10, 10);
}

void keyPressed() {
  ui.toggle();
}

void mousePressed() {
  ui.onPress(mouseX, mouseY);
}
