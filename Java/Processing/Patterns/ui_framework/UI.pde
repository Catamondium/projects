// Geometrically managed ui, both with O=0,0 relative to the UI implementor
public interface UI {
  void render(PGraphics canvas, float w, float h);
  void onPress(float x, float y, float w, float h);
}
