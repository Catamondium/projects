private class Layout implements UI {
  // Compositional button layout
  UI[] subs;
  PVector shape = new PVector();
  PVector dims = new PVector();
  PVector origin = new PVector();

  PVector porigin = new PVector();

  boolean showGrid = false;
  boolean visible = true;
  int margin;

  private Layout(PVector shape, int margin, PVector origin, PVector dims, UI[] subs) {
    this.shape = shape;
    this.origin = origin;
    this.porigin = origin;
    this.margin = margin;
    this.dims = dims;
    this.subs = subs;

    assert(this.shape.x * this.shape.y == subs.length);
  }

  public Layout grid(boolean state) {
    showGrid = state;
    return this;
  }

  public void show(boolean state) {
    visible = state;
  }

  public boolean shown() {
    return visible;
  }

  public void toggle() {
    visible = !visible;
  }

  private boolean isPressed(float x, float y, float ox, float oy, float w, float h) {
    return !(
      ((x < ox + origin.x + margin) || (y < oy + origin.y + margin)) || // too far upper-left
      ((x >= ox + origin.y + w - margin) || (y >= oy+ origin.y + h - margin)) // too far lower-right
      );
  }

  public void onPress(float x, float y) {
    if (!visible) return;
    // Cell dims
    float cw = dims.x / shape.x;
    float ch = dims.y / shape.y;
    for (int i = 0; i < shape.x; i++) {
      for (int j = 0; j < shape.y; j++) {
        int idx = floor(i + shape.x * j); // column-major index
        if (idx < subs.length && isPressed(x, y, i * cw, j * ch, cw, ch)) {
          // Translate clicks relative to UIs rendering origin
          float nx = x - i * cw - margin - origin.x;
          float ny = y - j * ch - margin - origin.x;
          subs[idx].onPress(nx, ny, cw - 2 * margin, ch - 2 * margin);
          break;
        }
      }
    }
  }

  public void onPress(float x, float y, float w, float h) {
    origin = new PVector(0, 0);
    dims = new PVector(w, h);
    this.onPress(x, y);
  }

  void renderGrid(PGraphics canvas) {
    canvas.pushStyle();
    canvas.noFill();
    //// Inner borders
    canvas.stroke(0, 255, 0); // GREEN
    float cw = dims.x / shape.x;
    float ch = dims.y / shape.y;
    for (int i = 0; i < shape.x; i++) {
      for (int j = 0; j < shape.y; j++) {
        canvas.rect(i * cw, j * ch, cw, ch);
      }
    }

    //// Outer border
    canvas.stroke(255, 0, 0); // RED;
    canvas.rect(0, 0, dims.x, dims.y);

    canvas.popStyle();
  }

  public void render(PGraphics pcanvas, float w, float h) {
    // Use geometry hints
    porigin = new PVector(0, 0);
    dims = new PVector(w, h);
    pcanvas.image(render(), porigin.x, porigin.y);
  }

  public PImage render() {
    PGraphics canvas = createGraphics(ceil(dims.x)+1, ceil(dims.y)+1);
    canvas.beginDraw();
    canvas.clear();
    // Cell dims
    if (visible) {
      float cw = dims.x / shape.x - margin;
      float ch = dims.y / shape.y - margin;
      for (int i = 0; i < shape.x; i++) {
        for (int j = 0; j < shape.y; j++) {
          int idx = floor(i + shape.x * j); // column-major index
          if (idx < subs.length) {
            canvas.pushStyle();
            canvas.pushMatrix();
            canvas.translate(i * cw + 2 * margin, j * ch + 2 * margin);

            // render w/o provided origin, UIs render with O=0,0
            subs[idx].render(canvas, cw - 2 * margin, ch - 2 * margin);

            canvas.popMatrix();
            canvas.popStyle();

            if (showGrid) {
              canvas.pushStyle();
              canvas.noFill();
              canvas.stroke(0, 0, 255); // BLUE
              canvas.rect(i * cw + 2 * margin, j * ch + 2 * margin, cw -  2 * margin, ch - 2 * margin);
              canvas.popStyle();
            }
          }
        }
      }
      if (showGrid)
        renderGrid(canvas);
    }
    canvas.endDraw();
    return canvas;
  }
}

private class LayoutBuilder {
  ArrayList<UI> subs = new ArrayList<UI>();
  PVector shape;
  PVector origin = new PVector(0, 0);
  PVector dims = new PVector(0, 0);
  int margin = 3;

  // Arbitrary grid
  LayoutBuilder(int x, int y) {
    shape = new PVector(x, y);
  }

  // Horizontal list
  LayoutBuilder(int len) {
    shape = new PVector(len, 1);
  }

  public LayoutBuilder setMargin(int px) {
    margin = px;
    return this;
  }

  public LayoutBuilder addUI(UI... fresh) {
    for (UI sub : fresh) {
      subs.add(sub);
    }
    return this;
  }

  public LayoutBuilder addButton(BClick cb) {
    return addUI(new Button(cb));
  }

  public LayoutBuilder addSlider(SClick cb) {
    return addUI(new Slider(cb));
  }

  public LayoutBuilder addToggle(TClick cb) {
    return addUI(new Toggle(cb));
  }

  public LayoutBuilder setGeo(int x, int y, int w, int h) {
    return this.setOrigin(x, y).setDims(w, h);
  }

  public LayoutBuilder setOrigin(int x, int y) {
    origin = new PVector(x, y);
    return this;
  }

  public LayoutBuilder setDims(int w, int h) {
    dims = new PVector(w, h);
    return this;
  }

  public Layout build() {
    assert(shape.x * shape.y == subs.size());
    return new Layout(shape, margin, origin, dims, subs.toArray(new UI[0]));
  }
};
