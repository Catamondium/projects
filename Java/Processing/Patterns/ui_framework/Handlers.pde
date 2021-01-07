// Simple Button handler
public interface BClick {
  void onPress();
}

// T consumer, void return
public interface Consumer<T> {
  void consume(T val);
}

// Slider response
public interface SClick extends Consumer<Float> {
}

// Toggle response
public interface TClick extends Consumer<Boolean> {
}

// objs holding values
public interface Supplier<T> {
  T supply();
}
