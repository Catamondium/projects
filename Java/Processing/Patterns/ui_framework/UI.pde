// Geometrically managed ui
public interface UI {
  void render(PGraphics canvas, float x, float y, float w, float h);
  void onPress(float x, float y, float w, float h);
}
