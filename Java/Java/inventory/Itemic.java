package inventory;

/**
 * A single item for stocking, or ordering.
 * <p>
 * Defaults to supplement stock requirements.
 * </p>
 */
public interface Itemic {
    boolean isOrder();

    default Itemic[] getPrecursors() {
        return new Itemic[0];
    }

    String getName();
    int getBatchWidth(); // Minimum stock multiple
    int getBatchCost();
    
    // Statistics capabilities
    /// Order some stock @param n quantity to order
    void request(int n);
    /// Data dump for report, clear stats
    void clear(); // TODO send String
    float mean();
    int median();
    int mode();
    int min();
    int max();
}