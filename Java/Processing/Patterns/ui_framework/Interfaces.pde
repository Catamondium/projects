public class Button implements UI {
  BClick listener;

  Button(BClick cb) {
    assert(cb != null);
    listener = cb;
  }

  void onPress(float _0, float _1, float _2, float _3) {
    listener.onPress();
  }

  void render(PGraphics canvas, float w, float h) {
    canvas.fill(255);
    canvas.rect(0, 0, w, h);
  }
}

public class Toggle implements UI, Supplier<Boolean> {
  TClick listener = null;
  boolean state = false;

  Toggle() {
  }

  Toggle(TClick cb) {
    listener = cb;
  }

  void onPress(float _0, float _1, float _2, float _3) {
    state = !state;
    if (listener != null)listener.consume(state);
  }

  Boolean supply() {
    return state;
  }

  void render(PGraphics canvas, float w, float h) {
    color c = state ? color(0, 255, 0) : color(255, 0, 0);
    canvas.fill(c);
    canvas.rect(0, 0, w, h);
  }
}

public class Slider implements UI, Supplier<Float> {
  SClick listener = null;
  float value;

  Slider() {
  }

  Slider(SClick cb) {
    listener = cb;
  }

  Float supply() {
    return value;
  }

  void onPress(float x, float _0, float w, float _1) {
    float nval = x / w;
    if (nval != value && listener != null) listener.consume(nval);
    value = nval;
  }

  void render(PGraphics canvas, float w, float h) {
    float sw = value * w;
    canvas.noFill();
    canvas.stroke(255);
    canvas.rect(0, 0, w, h);
    canvas.fill(0, 255, 0);
    canvas.rect(0, 0, sw, h);
  }
}
